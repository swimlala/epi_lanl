CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-01T03:15:57Z AOML 3.0 creation; 2016-08-07T21:36:43Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160201031557  20160807143643  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               bA   AO  5286_8897_098                   2C  D   APEX                            6531                            072314                          846 @ג=R�]`1   @ג>  �@3N��O�;�c4Z�11   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    bA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D�	�D�@ D��fD�� D���D�C3D�p D��fD�3D�33D�� D��fD� D�33D�c3D���D�3D�9�D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @A�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C :�C!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH:�CJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDG�DG��DHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\��D]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt{�Dya�D��D�D)D���D��)D� �D�G\D�t)D�ڏD�\D�7\D��)D�ڏD�)D�7\D�g\D���D�\D�=�D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aȟ�AȑhAȏ\AȍPAȃA�x�A�r�A�p�A�p�A�t�A�x�A�p�A�hsA�bNA�XA�VA�S�A�S�A�XA�^5A�ffAȁAȟ�Aȝ�Aȥ�Aȩ�AȺ^A��HAȝ�A�oA�1A�`BA��A�l�Aé�A�\)Aô9A��A��A�oA��A�=qA�;dA�-A�bA��mAÕ�A�O�A�E�A�S�A�jA�;dA�;dA��A�p�A�ffA�9XA��HA�^5A��A�Q�A��A�bNA�(�A��A�$�A�`BA�ƨA���A�~�A�S�A�^5A���A���A�n�A���A���A�S�A��+A�7LA�"�A���A�bA�JA���A�n�A���A���A��A�+A�^5A�dZA~�/A|z�AxffAw�
AtffAo�Ao��Aj1'Ab�\A\I�AX��AU�#AR  AQt�AQVAOK�ALA�AJ�AH��AF��ADz�ACO�AAx�A?t�A?7LA>��A>z�A<��A:�!A9��A8�yA6�RA6n�A6=qA5�mA4��A3x�A2�HA0�A/A-�A+�A*JA)�A(�jA(ZA'�FA&��A%"�A!7LA33Av�A$�A�A�wAS�A��A�yA��Ap�A�A�!An�A$�A�A��A�PAr�A��AE�A��A��A�mA��AVA�A7LA��A
��A	p�A�A�!A�AS�A�mA�jAt�A E�@���@�@��@�/@�1@�@�ff@��7@��@�  @�|�@�5?@���@��@�@�;d@�ff@�Ĝ@��T@�D@���@�o@��@�M�@���@�-@��@�E�@�A�@���@���@�1@۝�@��;@�ff@ج@׶F@���@�V@�=q@�1'@�33@���@�hs@�7L@�&�@�&�@���@�bN@� �@��;@�l�@�33@���@���@�X@�hs@�V@���@���@�/@���@ȴ9@ț�@��@�\)@��@�n�@�x�@���@ēu@�(�@Å@���@�=q@��@�Q�@��;@�|�@�;d@��\@�X@�C�@�M�@���@���@��@�C�@��#@��/@�j@�t�@�"�@�M�@�V@��@�I�@���@��
@�t�@��@�E�@�`B@���@�b@���@�;d@�"�@��H@���@�^5@�J@�7L@��@��@��@��@��@�Ĝ@�ƨ@�t�@��@��\@�n�@�~�@���@��7@���@�  @���@�dZ@��@�^5@�5?@��@���@�&�@�V@�%@���@�Q�@��@���@�33@��R@�~�@�ff@�-@���@�p�@�O�@���@���@�1'@�1'@���@�hs@��\@��R@��@��@�\)@�-@��T@��-@���@��7@�x�@�`B@�x�@�G�@��u@��@�|�@��y@�n�@��^@��@�G�@�/@��-@��-@��7@�hs@�X@�?}@�bN@��@�S�@�S�@�@�$�@��@��T@��@�=q@�^5@�^5@��\@���@�n�@�M�@��\@���@��+@���@��@�Ĝ@��9@�A�@���@�33@���@�ff@�5?@���@��^@��^@���@�G�@�%@��@��j@��@���@�l�@�dZ@�\)@�S�@�33@��@��y@�ff@�@��@��-@��7@�x�@�hs@�`B@�7L@��`@��/@���@��F@���@�ff@���@�n�@�ff@�^5@�M�@�$�@���@��-@�@�@���@���@��7@�G�@���@���@��9@��u@�Z@�(�@�(�@�9X@�1@���@�t�@�C�@�+@�o@�
=@��@��\@�V@��@��T@��^@��@�G�@�7L@��@��@�j@�P@~��@~��@~�+@~{@}��@}?}@|�/@|�@|��@{�m@{C�@z��@y�@y��@y��@y��@y��@yX@xr�@w��@tZ@k33@a%@X��@SC�@MO�@G\)@?|�@8��@3�m@/�P@(��@#�F@ b@�H@��@��@�/@	�@E�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aȟ�AȑhAȏ\AȍPAȃA�x�A�r�A�p�A�p�A�t�A�x�A�p�A�hsA�bNA�XA�VA�S�A�S�A�XA�^5A�ffAȁAȟ�Aȝ�Aȥ�Aȩ�AȺ^A��HAȝ�A�oA�1A�`BA��A�l�Aé�A�\)Aô9A��A��A�oA��A�=qA�;dA�-A�bA��mAÕ�A�O�A�E�A�S�A�jA�;dA�;dA��A�p�A�ffA�9XA��HA�^5A��A�Q�A��A�bNA�(�A��A�$�A�`BA�ƨA���A�~�A�S�A�^5A���A���A�n�A���A���A�S�A��+A�7LA�"�A���A�bA�JA���A�n�A���A���A��A�+A�^5A�dZA~�/A|z�AxffAw�
AtffAo�Ao��Aj1'Ab�\A\I�AX��AU�#AR  AQt�AQVAOK�ALA�AJ�AH��AF��ADz�ACO�AAx�A?t�A?7LA>��A>z�A<��A:�!A9��A8�yA6�RA6n�A6=qA5�mA4��A3x�A2�HA0�A/A-�A+�A*JA)�A(�jA(ZA'�FA&��A%"�A!7LA33Av�A$�A�A�wAS�A��A�yA��Ap�A�A�!An�A$�A�A��A�PAr�A��AE�A��A��A�mA��AVA�A7LA��A
��A	p�A�A�!A�AS�A�mA�jAt�A E�@���@�@��@�/@�1@�@�ff@��7@��@�  @�|�@�5?@���@��@�@�;d@�ff@�Ĝ@��T@�D@���@�o@��@�M�@���@�-@��@�E�@�A�@���@���@�1@۝�@��;@�ff@ج@׶F@���@�V@�=q@�1'@�33@���@�hs@�7L@�&�@�&�@���@�bN@� �@��;@�l�@�33@���@���@�X@�hs@�V@���@���@�/@���@ȴ9@ț�@��@�\)@��@�n�@�x�@���@ēu@�(�@Å@���@�=q@��@�Q�@��;@�|�@�;d@��\@�X@�C�@�M�@���@���@��@�C�@��#@��/@�j@�t�@�"�@�M�@�V@��@�I�@���@��
@�t�@��@�E�@�`B@���@�b@���@�;d@�"�@��H@���@�^5@�J@�7L@��@��@��@��@��@�Ĝ@�ƨ@�t�@��@��\@�n�@�~�@���@��7@���@�  @���@�dZ@��@�^5@�5?@��@���@�&�@�V@�%@���@�Q�@��@���@�33@��R@�~�@�ff@�-@���@�p�@�O�@���@���@�1'@�1'@���@�hs@��\@��R@��@��@�\)@�-@��T@��-@���@��7@�x�@�`B@�x�@�G�@��u@��@�|�@��y@�n�@��^@��@�G�@�/@��-@��-@��7@�hs@�X@�?}@�bN@��@�S�@�S�@�@�$�@��@��T@��@�=q@�^5@�^5@��\@���@�n�@�M�@��\@���@��+@���@��@�Ĝ@��9@�A�@���@�33@���@�ff@�5?@���@��^@��^@���@�G�@�%@��@��j@��@���@�l�@�dZ@�\)@�S�@�33@��@��y@�ff@�@��@��-@��7@�x�@�hs@�`B@�7L@��`@��/@���@��F@���@�ff@���@�n�@�ff@�^5@�M�@�$�@���@��-@�@�@���@���@��7@�G�@���@���@��9@��u@�Z@�(�@�(�@�9X@�1@���@�t�@�C�@�+@�o@�
=@��@��\@�V@��@��T@��^@��@�G�@�7L@��@��@�j@�P@~��@~��@~�+@~{@}��@}?}@|�/@|�@|��@{�m@{C�@z��@y�@y��@y��@y��@y��@yX@xr�G�O�@tZ@k33@a%@X��@SC�@MO�@G\)@?|�@8��@3�m@/�P@(��@#�F@ b@�H@��@��@�/@	�@E�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBƨBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBƨBƨBǮB��B��B�B�B	#�B	ZB	jB	n�B	� B	��B	�LB	�?B
PB
<jB
� B
�3B
ŢB
ƨB
��B$�B'�B,B;dBW
BbNBffBn�Bv�Bv�Bt�B|�B�1B�uB�hB��B��B��B�;B�B%B-B5?B/BH�BjBjBS�BC�B-BK�B��B�'B�jB�}B�wB�B�^B�}B��B�BB�B�RB�PBC�B
��B
�B
DB	�#B	�LB	�FB	�B	��B	�B	��B	��B	y�B	p�B	XB	B�B	;dB	(�B	%B�TB��B�FB�B�!B�9B�?B��B�Bz�Bx�B�%B~�Br�Bn�Bn�Bn�Bl�BiyBdZBbNB^5B^5Be`B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B��B��B��B��B�uB~�BffBbNB_;B]/BZBZBYBYBYBYBYBVBVBW
BXB\)B_;B_;B^5B^5B\)B^5B_;BbNBffBiyBiyBffBdZBgmBffBgmBk�Bp�Bs�Bq�Bo�Bs�Bt�Bw�Bx�Bx�Bw�Bx�B�B�B�1B�PB�VB�JB�%B�B�B~�By�Bt�Bs�Bn�BiyBjBm�Bo�Bu�By�By�B� B}�B�B�B�7B�1B�%B�B�B�B�%B�7B�7B�DB�PB�\B�hB��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�?B�3B�3B�?B�XB�XB�XB�XB�qB�}BBĜBƨBǮBȴBȴBɺBɺBȴB��B��B��B��B�B�B�HB�TB�sB�yB�B��B��B��B��B��B��B	B	+B	VB	oB	�B	�B	�B	�B	�B	 �B	!�B	$�B	)�B	-B	-B	0!B	49B	5?B	5?B	7LB	7LB	<jB	?}B	B�B	G�B	J�B	J�B	K�B	M�B	N�B	N�B	Q�B	XB	_;B	bNB	bNB	e`B	gmB	gmB	jB	n�B	p�B	r�B	u�B	y�B	z�B	z�B	{�B	~�B	�B	�B	�B	�+B	�7B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�FB	�FB	�?B	�?B	�RB	�^B	�wB	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�HB	�TB	�TB	�TB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
%B
1B
1B
1B
+B
1B
1B
	7B

=B

=B
	7B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
\B
hB
uB
�B
$�B
-B
49B
7LB
>wB
D�B
J�B
O�B
S�B
ZB
^5B
`BB
e`B
jB
n�B
t�B
w�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BƱBŬBŭBŬBŭBŭBŬBŮBŭBůBŮBŮBŭBŭBŭBƳBƳBǵB��B��B�B�B	#�B	Z"B	j�B	n�B	�B	��B	�OB	�BB
MB
<gB
�B
�,B
ŘB
ƟB
��B$�B'�B, B;ZBWBbBBf\Bn�Bv�Bv�Bt�B|�B�&B�oB�]B��B��B˼B�0B�uBB-B55B/BH�BjsBjxBS�BC�B-BK�B��B�B�bB�qB�lB�B�SB�sB��B�B �B�B�EB�BBC�B
��B
�B
=B	�B	�JB	�DB	�B	��B	�B	��B	��B	y�B	p�B	XB	B�B	;cB	(�B	)B�YB��B�KB�
B�'B�@B�FB��B�'Bz�Bx�B�-BBr�Bn�Bn�Bn�Bl�Bi�BdbBbWB^>B^@BeiB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�eB��B��B��B��B�{BBfnBbVB_AB]6BZ&BZ&BYBYBYBYBYBVBVBWBXB\1B_DB_BB^<B^;B\0B^<B_@BbUBfoBiBiBfoBd_BgrBfnBgsBk�Bp�Bs�Bq�Bo�Bs�Bt�Bw�Bx�Bx�Bw�Bx�B�B�#B�6B�UB�YB�MB�)B�B�B~�By�Bt�Bs�Bn�BiBj�Bm�Bo�Bu�By�By�B�B}�B�B�B�<B�3B�*B�#B�B�B�)B�<B�<B�JB�TB�cB�lB��B��B��B��B��B��B��B��B�B�	B�B�"B�(B�.B�@B�4B�4B�CB�XB�YB�WB�ZB�pB�BBğBƦBǬBȳBȳBɾBɼBȶB��B��B��B��B�B�B�GB�TB�sB�xB�B��B��B��B��B��B��B	B	*B	UB	lB	�B	�B	�B	�B	�B	 �B	!�B	$�B	)�B	-
B	-
B	0B	46B	5;B	5;B	7FB	7HB	<fB	?yB	B�B	G�B	J�B	J�B	K�B	M�B	N�B	N�B	Q�B	XB	_5B	bHB	bFB	eZB	gfB	ghB	jxB	n�B	p�B	r�B	u�B	y�B	z�B	z�B	{�B	~�B	��B	�B	�B	�#B	�0B	�IB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�+B	�;B	�=B	�6B	�8B	�GB	�VB	�pB	�{B	ēB	ɰB	ʸB	ʷB	˽B	��B	��B	˽B	��B	��B	��B	��B	��B	��B	�B	�%B	�,B	�1B	�>B	�JB	�JB	�KB	�]B	�jB	�rB	�uB	�sB	�sB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
 B
B
B
B
B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
(B
%B
%B
"B
'B
%B
	.B

3B

1B
	,B
%B
	,B
	,B

0B

3B

2B
:B
9B
8B
9B
@B
?B
>B
AB
FB
EB
EB
KB
QG�O�B
kB
�B
$�B
,�B
4-B
7>B
>kB
D�B
J�B
O�B
S�B
ZB
^'B
`5B
eRB
jqB
n�B
t�B
w�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436432016080714364320160807143643  AO  ARCAADJP                                                                    20160201031557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160201031557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160201031557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143643  IP                  G�O�G�O�G�O�                