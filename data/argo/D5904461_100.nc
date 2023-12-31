CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-11T20:18:04Z AOML 3.0 creation; 2016-08-07T21:36:43Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160211201804  20160807143643  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               dA   AO  5286_8897_100                   2C  D   APEX                            6531                            072314                          846 @ה�u�1   @ה��|�@3/\(��cF=p��
1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    dA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B�33B�ffBÙ�B�  B�  B�ffBә�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D��D�@ D���D�� D�  D�,�D��3D���D�	�D�C3D�p D��3D���D�FfD�i�D���D�fD�P D�fD�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @A�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B���B�B�B�\B�B�B�B�B�B�B�B�B�u�B���B��)B�B�B�B�BШ�B��)B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDtn�Dy�D� �D�D)D���D��)D�)D�0�D��\D���D��D�G\D�t)D��\D� �D�J�D�m�D���D��D�T)D�D�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�;dA�M�A�O�A�O�A�Q�A�S�A�S�A�VA�S�A�S�A�VA�VA�VA�VA�\)A�VA�VA�ZA�\)A�^5A�^5A�\)A�\)A�^5A�XA�dZA�jA�p�A�r�A�t�A�v�A�x�AǕ�A���A���Aǰ!Aǥ�AǛ�AǶFAǴ9A�9XAƁAŗ�A�x�AÇ+A���A�r�A��A�;dA��^A��A��mA�ƨA��`A�jA�{A��;A�JA�ĜA��9A���A��DA�dZA���A��yA�C�A���A���A��-A�A�A���A��A��A�hsA�K�A�{A���A�hsA�wA|I�Ax(�At^5Ap-An  Al�`Ah�Ae�TAc�A^$�A[|�AYx�AV1'AT{AQ��APr�AN�AM��ALQ�AJA�AH�AC&�AAK�A@jA>A�A<JA9��A7oA4��A1t�A/��A.5?A,E�A+ƨA*��A*E�A*  A)?}A'�A'�#A'��A'ƨA%��A$�A"A�A!
=A!&�A ��A�^AƨAbA�FA~�A�/A�hA{A�A�A��A~�A��A�DA�A7LA��A��A��Al�AA�A
�9A
VA
�A�DAoA��Av�A-A��A^5A�TA��A`BA ĜA ��A j@���@�t�@�J@�Ĝ@�  @�dZ@�@�!@�v�@�-@���@�^@�p�@�&�@�%@��
@�u@�\@�v�@�n�@�5?@���@�p�@�j@��@�  @�ȴ@�^5@���@�p�@���@�A�@ݲ-@�C�@�~�@��`@��@�\)@���@�=q@���@�Ĝ@ӶF@�E�@�G�@�Q�@϶F@��H@���@�/@�I�@˝�@��@ʟ�@�5?@�7L@���@ċD@��@Ý�@�S�@�33@��y@�V@�/@��9@��@���@�V@�O�@��`@�r�@��@�dZ@��+@��@��@�Z@��m@���@�\)@��@�v�@�J@��-@�`B@��j@���@�33@�V@��@��@���@�%@�j@�b@��@���@���@���@�E�@��@�J@���@���@�X@�&�@���@�Z@�1'@��m@�t�@�+@���@��!@��\@�^5@��@��7@���@��@�Z@���@�|�@�"�@��R@�v�@�n�@�n�@���@���@��@�hs@��@�Ĝ@�j@�(�@�|�@��R@�^5@�-@���@�@�hs@��@�j@�Z@� �@��m@�C�@�+@�o@�@���@�^5@�5?@�{@��@�@��h@�/@�Ĝ@��9@���@��@�r�@�bN@�Q�@�1'@�1@��P@�K�@�;d@�"�@��y@�ȴ@���@���@�n�@�V@�=q@�-@���@��^@���@�p�@�&�@��@��@�r�@�z�@��@�9X@��@���@��w@��F@���@�+@�
=@���@�=q@���@��h@���@�Ĝ@���@��w@���@��9@���@�j@�1'@��@��@��H@���@��@�@�x�@�X@�V@��u@�z�@�r�@�Z@��@���@��@�dZ@��@���@���@�=q@�$�@��-@�hs@���@��@�j@�9X@��;@���@�t�@�K�@��@���@���@���@�ff@�{@��^@�X@��@�&�@�%@��/@���@�A�@�1@��;@�ƨ@��F@��F@��F@��F@��@���@��@�l�@�
=@��H@��@���@��!@��\@��+@�v�@�M�@�@���@���@��7@�`B@�7L@��@���@���@��j@��@�I�@� �@��@���@���@��@��@�dZ@�+@��@�ȴ@���@���@��+@�~�@�^5@�{@��#@���@�p�@�p�@�p�@�hs@�O�@�%@��D@�9X@l�@~ȴ@~5?@}�-@}O�@}?}@}�@x��@o\)@e/@\�D@R�!@N@Fȴ@@  @7�;@1��@+��@'
=@"�!@l�@=q@��@��@Z@��@�j@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�-A�;dA�M�A�O�A�O�A�Q�A�S�A�S�A�VA�S�A�S�A�VA�VA�VA�VA�\)A�VA�VA�ZA�\)A�^5A�^5A�\)A�\)A�^5A�XA�dZA�jA�p�A�r�A�t�A�v�A�x�AǕ�A���A���Aǰ!Aǥ�AǛ�AǶFAǴ9A�9XAƁAŗ�A�x�AÇ+A���A�r�A��A�;dA��^A��A��mA�ƨA��`A�jA�{A��;A�JA�ĜA��9A���A��DA�dZA���A��yA�C�A���A���A��-A�A�A���A��A��A�hsA�K�A�{A���A�hsA�wA|I�Ax(�At^5Ap-An  Al�`Ah�Ae�TAc�A^$�A[|�AYx�AV1'AT{AQ��APr�AN�AM��ALQ�AJA�AH�AC&�AAK�A@jA>A�A<JA9��A7oA4��A1t�A/��A.5?A,E�A+ƨA*��A*E�A*  A)?}A'�A'�#A'��A'ƨA%��A$�A"A�A!
=A!&�A ��A�^AƨAbA�FA~�A�/A�hA{A�A�A��A~�A��A�DA�A7LA��A��A��Al�AA�A
�9A
VA
�A�DAoA��Av�A-A��A^5A�TA��A`BA ĜA ��A j@���@�t�@�J@�Ĝ@�  @�dZ@�@�!@�v�@�-@���@�^@�p�@�&�@�%@��
@�u@�\@�v�@�n�@�5?@���@�p�@�j@��@�  @�ȴ@�^5@���@�p�@���@�A�@ݲ-@�C�@�~�@��`@��@�\)@���@�=q@���@�Ĝ@ӶF@�E�@�G�@�Q�@϶F@��H@���@�/@�I�@˝�@��@ʟ�@�5?@�7L@���@ċD@��@Ý�@�S�@�33@��y@�V@�/@��9@��@���@�V@�O�@��`@�r�@��@�dZ@��+@��@��@�Z@��m@���@�\)@��@�v�@�J@��-@�`B@��j@���@�33@�V@��@��@���@�%@�j@�b@��@���@���@���@�E�@��@�J@���@���@�X@�&�@���@�Z@�1'@��m@�t�@�+@���@��!@��\@�^5@��@��7@���@��@�Z@���@�|�@�"�@��R@�v�@�n�@�n�@���@���@��@�hs@��@�Ĝ@�j@�(�@�|�@��R@�^5@�-@���@�@�hs@��@�j@�Z@� �@��m@�C�@�+@�o@�@���@�^5@�5?@�{@��@�@��h@�/@�Ĝ@��9@���@��@�r�@�bN@�Q�@�1'@�1@��P@�K�@�;d@�"�@��y@�ȴ@���@���@�n�@�V@�=q@�-@���@��^@���@�p�@�&�@��@��@�r�@�z�@��@�9X@��@���@��w@��F@���@�+@�
=@���@�=q@���@��h@���@�Ĝ@���@��w@���@��9@���@�j@�1'@��@��@��H@���@��@�@�x�@�X@�V@��u@�z�@�r�@�Z@��@���@��@�dZ@��@���@���@�=q@�$�@��-@�hs@���@��@�j@�9X@��;@���@�t�@�K�@��@���@���@���@�ff@�{@��^@�X@��@�&�@�%@��/@���@�A�@�1@��;@�ƨ@��F@��F@��F@��F@��@���@��@�l�@�
=@��H@��@���@��!@��\@��+@�v�@�M�@�@���@���@��7@�`B@�7L@��@���@���@��j@��@�I�@� �@��@���@���@��@��@�dZ@�+@��@�ȴ@���@���@��+@�~�@�^5@�{@��#@���@�p�@�p�@�p�@�hs@�O�@�%@��D@�9X@l�@~ȴ@~5?@}�-@}O�@}?}G�O�@x��@o\)@e/@\�D@R�!@N@Fȴ@@  @7�;@1��@+��@'
=@"�!@l�@=q@��@��@Z@��@�j@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	+B	hB	�B	�B	#�B	&�B	-B	2-B	;dB	G�B	L�B	I�B	K�B	L�B	W
B	�B	ɺB
�B
VB
�=B
�B
��B
`BB
G�B
gmB
p�B
/B
I�B
T�B
+B
1B
B
49B
YB
� B
��B
��B
�B
�/B
�B
ƨB
�oB
^5B
G�B
:^B
2-B
.B
-B
,B
"�B

=B	�`B	ĜB	��B	�7B	gmB	L�B	;dB	/B	'�B	 �B	{B	PB	%B��B�B�BB��B��B��B�dB�FB�-B��B��B��B�bB�JB�7B�B� Bx�Bn�BbNB_;BaHBdZBx�B~�B�B�B�1B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B�{B�bB�JB�JB�DB�7B�1B�=B�1B�B�+B�=B�=B�7B|�By�B~�B� B�B�B�B�B�B�B�B�B�B�B�B�B� B~�B~�B� B�B~�By�By�B�B�+B�1B�DB�JB�JB�=B�1B�%B�%B�B�%B�7B�7B�=B�=B�7B�JB�\B�oB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�?B�^B�}B��BBŢBǮB��B��B��B��B��B��B�B�#B�)B�;B�TB�ZB�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	+B	
=B	JB	VB	hB	oB	�B	�B	�B	�B	!�B	'�B	+B	-B	.B	2-B	49B	5?B	7LB	;dB	>wB	A�B	D�B	E�B	D�B	G�B	J�B	K�B	K�B	N�B	Q�B	W
B	ZB	^5B	bNB	dZB	dZB	e`B	gmB	jB	n�B	o�B	p�B	s�B	s�B	w�B	x�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�JB	�JB	�JB	�VB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�?B	�LB	�LB	�LB	�RB	�jB	��B	B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B

=B

=B
DB
DB
JB
JB
PB
hB
�B
 �B
)�B
2-B
6FB
<jB
B�B
K�B
Q�B
VB
YB
^5B
aHB
ffB
l�B
q�B
t�B
x�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	$B	3B	pB	�B	�B	#�B	&�B	-B	22B	;kB	G�B	L�B	I�B	K�B	L�B	WB	� B	ɹB
�B
V B
�8B
�B
�zB
`?B
G�B
gfB
p�B
/B
I�B
T�B
*�B
,B
	B
44B
YB
�B
��B
˻B
�B
�%B
�B
ƟB
�eB
^+B
G�B
:VB
2&B
.B
-B
,B
"�B

8B	�^B	ęB	��B	�7B	glB	L�B	;fB	/B	'�B	 �B	~B	UB	)B��B�B�FB��B��B��B�lB�KB�5B�B��B��B�kB�PB�?B�'B�	Bx�Bn�BbZB_EBaTBdbBx�BB�B�'B�:B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�iB�NB�OB�HB�=B�6B�DB�7B�B�1B�BB�BB�<B|�By�B~�B�B�B�B�B�B�B�B� B�B�$B� B�B�B�B~�B B�B�B~�By�By�B�B�0B�6B�IB�MB�NB�AB�8B�'B�*B�%B�)B�>B�<B�@B�AB�;B�OB�`B�rB�mB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�4B�AB�]B�~B��BBŢBǮB��B��B��B��B��B��B�B�#B�*B�<B�RB�[B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	(B	
;B	FB	UB	fB	mB	zB	�B	�B	�B	!�B	'�B	*�B	-B	.B	2(B	45B	5;B	7IB	;bB	>rB	A�B	D�B	E�B	D�B	G�B	J�B	K�B	K�B	N�B	Q�B	WB	ZB	^.B	bGB	dUB	dSB	e[B	geB	jyB	n�B	o�B	p�B	s�B	s�B	w�B	x�B	y�B	z�B	|�B	~�B	� B	�B	�	B	�B	�B	�B	�B	�$B	�0B	�7B	�<B	�BB	�CB	�DB	�NB	�_B	�hB	�gB	�mB	�{B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B	�3B	�6B	�5B	�BB	�CB	�DB	�JB	�bB	�}B	B	ƟB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�&B	�0B	�<B	�CB	�RB	�NB	�VB	�\B	�dB	�iB	�jB	�iB	�hB	�jB	�mB	�nB	�oB	�oB	�sB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
!B
B
'B

2B

1B
:B
8B
>B
=G�O�B
]B
|B
 �B
)�B
2"B
6:B
<[B
B�B
K�B
Q�B
U�B
YB
^)B
a7B
fXB
l~B
q�B
t�B
x�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436432016080714364320160807143643  AO  ARCAADJP                                                                    20160211201804    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160211201804  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160211201804  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143643  IP                  G�O�G�O�G�O�                