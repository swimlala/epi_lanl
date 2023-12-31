CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-04T09:17:02Z AOML 3.0 creation; 2016-08-07T21:36:45Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160404091702  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               nA   AO  5286_8897_110                   2C  D   APEX                            6531                            072314                          846 @ע!�1   @ע�h�@3A$�/�c\�`A�71   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    nA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C�fC�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�fD�FfD�� D��fD�3D�9�D���D�ٚD�  D�L�D�l�D��3D�	�D�0 D�p D�ɚD�  D�9�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C :�C!HC�C�C�C
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>��D?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt��Dy��D�
�D�J�D��)D�ʏD�\D�=�D���D���D�)D�P�D�p�D��\D��D�4)D�t)D���D�)D�=�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˲-A˲-A˾wA�ĜA�ĜA˾wA�|�A�bNA�?}A���A��/A��
A���A���A�ȴA�A�ĜA�ƨA�ȴA���A���A���A���A���A��HA��yA�JA�;dA�n�A�v�A�jA�dZA��Aʧ�A�$�A���Aɴ9A�v�A�&�A�ƨAȥ�Aȟ�A��#A��A���A��yAĥ�Ać+A� �A×�A�9XA²-A�ZA��A�ffA�+A���A�S�A�v�A��/A��A��\A�=qA��9A��DA�=qA��yA���A��A��RA�I�A�(�A�jA��A�K�A��#A���A�&�A���A�  A��9A���A�bNA�A��uA�p�A��DA���A�5?A���A�33A�S�A�33A�ȴA��PA��A�A�jA���A��A�=qA��A�`BA�oA�ĜA��A��HA��A�33A~ĜAz�`Aup�As%AqAjA�Ah�9Af�Af1'Ae
=Ab�Aa;dA^��AZĜAX��ATffASƨAO�;ANv�AM��AK�AI�AH �AG
=AEdZADAB�\AA��AA7LA@5?A?�-A?`BA>�A=��A;�hA:��A9�A6�A4��A1�A/XA/oA.�A.��A/�A.Q�A-|�A,ȴA,�A+A*�/A)�A'��A&$�A$�DA$(�A"��A!��A �A��A%A�AM�AS�AƨA��A �A�
A�#A?}AK�A�9At�AA�DA^5A\)A\)AAAv�AS�A^5A��Ax�A
�yA
Q�A	;dA�A��A`BA^5A1A��At�A�+A/A �j@��@�-@��#@���@�G�@��`@�n�@��@�bN@�n�@�dZ@�ff@�&�@�9X@�&�@��H@�/@�V@�&�@睲@�7@��@��@�+@�\@��/@�R@�bN@ߍP@�K�@�v�@�(�@�ƨ@�;d@��`@�1@���@�n�@��#@��/@���@�j@� �@���@�|�@��y@�5?@��T@Ѻ^@љ�@�p�@�`B@�/@д9@ЋD@�b@ϝ�@ϝ�@�o@�A�@�ff@��@�p�@ȼj@���@�n�@�Z@�ƨ@�K�@���@�n�@�J@��@���@�`B@�-@�@�@�~�@�^5@��^@��D@���@��u@�A�@���@�t�@�C�@�33@�o@�n�@��@��^@���@�z�@��
@��@��@��-@��@�Q�@���@��u@���@��y@���@�"�@�"�@�o@��@�ff@��@���@�/@�?}@�O�@���@�9X@�1@��@���@�7L@��;@�1'@�z�@��/@�V@�&�@�G�@�hs@���@�7L@���@�z�@��j@���@���@��@��@�n�@�$�@�5?@���@�G�@���@���@�Z@��9@���@�Q�@�|�@�@���@�?}@���@��u@���@�|�@�;d@��!@�ff@�p�@�/@�%@��`@���@�bN@��;@�S�@�ȴ@��\@�n�@�J@��@��-@�p�@�V@���@��/@��u@�9X@�I�@�l�@�K�@��@�I�@�I�@��@�5?@�$�@�=q@��T@��7@�1'@���@���@�|�@�S�@�C�@�+@��@���@�^5@�M�@��@�@�@�@�@�@�J@�$�@�v�@�~�@�n�@�^5@��+@���@���@��+@�v�@�ȴ@��H@��@��@��y@�~�@�V@��@���@�Q�@�1'@��@��P@�~�@��T@��^@�X@�/@��9@�(�@���@�C�@�
=@�-@���@�@���@��7@���@���@�z�@�bN@�A�@�b@��w@�|�@�;d@�o@��@���@���@�^5@��#@��7@�G�@�?}@��@���@��D@�bN@� �@��;@��@��@�dZ@���@�~�@�n�@�$�@�J@�@� �@� �@y�7@o��@b=q@X�9@QX@MO�@F{@@�9@8��@2�H@+S�@'\)@!��@�@��@1@��@��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A˲-A˲-A˾wA�ĜA�ĜA˾wA�|�A�bNA�?}A���A��/A��
A���A���A�ȴA�A�ĜA�ƨA�ȴA���A���A���A���A���A��HA��yA�JA�;dA�n�A�v�A�jA�dZA��Aʧ�A�$�A���Aɴ9A�v�A�&�A�ƨAȥ�Aȟ�A��#A��A���A��yAĥ�Ać+A� �A×�A�9XA²-A�ZA��A�ffA�+A���A�S�A�v�A��/A��A��\A�=qA��9A��DA�=qA��yA���A��A��RA�I�A�(�A�jA��A�K�A��#A���A�&�A���A�  A��9A���A�bNA�A��uA�p�A��DA���A�5?A���A�33A�S�A�33A�ȴA��PA��A�A�jA���A��A�=qA��A�`BA�oA�ĜA��A��HA��A�33A~ĜAz�`Aup�As%AqAjA�Ah�9Af�Af1'Ae
=Ab�Aa;dA^��AZĜAX��ATffASƨAO�;ANv�AM��AK�AI�AH �AG
=AEdZADAB�\AA��AA7LA@5?A?�-A?`BA>�A=��A;�hA:��A9�A6�A4��A1�A/XA/oA.�A.��A/�A.Q�A-|�A,ȴA,�A+A*�/A)�A'��A&$�A$�DA$(�A"��A!��A �A��A%A�AM�AS�AƨA��A �A�
A�#A?}AK�A�9At�AA�DA^5A\)A\)AAAv�AS�A^5A��Ax�A
�yA
Q�A	;dA�A��A`BA^5A1A��At�A�+A/A �j@��@�-@��#@���@�G�@��`@�n�@��@�bN@�n�@�dZ@�ff@�&�@�9X@�&�@��H@�/@�V@�&�@睲@�7@��@��@�+@�\@��/@�R@�bN@ߍP@�K�@�v�@�(�@�ƨ@�;d@��`@�1@���@�n�@��#@��/@���@�j@� �@���@�|�@��y@�5?@��T@Ѻ^@љ�@�p�@�`B@�/@д9@ЋD@�b@ϝ�@ϝ�@�o@�A�@�ff@��@�p�@ȼj@���@�n�@�Z@�ƨ@�K�@���@�n�@�J@��@���@�`B@�-@�@�@�~�@�^5@��^@��D@���@��u@�A�@���@�t�@�C�@�33@�o@�n�@��@��^@���@�z�@��
@��@��@��-@��@�Q�@���@��u@���@��y@���@�"�@�"�@�o@��@�ff@��@���@�/@�?}@�O�@���@�9X@�1@��@���@�7L@��;@�1'@�z�@��/@�V@�&�@�G�@�hs@���@�7L@���@�z�@��j@���@���@��@��@�n�@�$�@�5?@���@�G�@���@���@�Z@��9@���@�Q�@�|�@�@���@�?}@���@��u@���@�|�@�;d@��!@�ff@�p�@�/@�%@��`@���@�bN@��;@�S�@�ȴ@��\@�n�@�J@��@��-@�p�@�V@���@��/@��u@�9X@�I�@�l�@�K�@��@�I�@�I�@��@�5?@�$�@�=q@��T@��7@�1'@���@���@�|�@�S�@�C�@�+@��@���@�^5@�M�@��@�@�@�@�@�@�J@�$�@�v�@�~�@�n�@�^5@��+@���@���@��+@�v�@�ȴ@��H@��@��@��y@�~�@�V@��@���@�Q�@�1'@��@��P@�~�@��T@��^@�X@�/@��9@�(�@���@�C�@�
=@�-@���@�@���@��7@���@���@�z�@�bN@�A�@�b@��w@�|�@�;d@�o@��@���@���@�^5@��#@��7@�G�@�?}@��@���@��D@�bN@� �@��;@��@��@�dZ@���@�~�@�n�@�$�@�JG�O�@� �@� �@y�7@o��@b=q@X�9@QX@MO�@F{@@�9@8��@2�H@+S�@'\)@!��@�@��@1@��@��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	B	1B	�B	�B	9XB	ZB	w�B	�1B	��B	�XB	�B	�#B	��B	��B	ĜB	�^B	�B	��B	�RB	��B	�RB	ŢB
uB
aHB
t�B
~�B
��B
�^B
ŢB
�
B
�TB
��B  BBBJB�B"�BI�B�-B��B%�BD�BS�BG�B0!B-B%�B!�B	7B%BB��B�B�B�B�sB�;B�B�B��B��BĜB�'B��B�B{�Bz�B�Bs�B6FB:^BA�B<jB.B#�B
��B
�TB
��B
��B
�9B
��B
ffB
F�B
33B
{B
B	��B	�#B	��B	{�B	aHB	8RB	/B	'�B	$�B	"�B	&�B	�B	�B	B��B�mB�BB��BŢB��B�dB�LB�?B�?B�!B�B�B�B�!B�B�B��B��B��B�B�FB�dB�RB�3B�B��B��B��B�B��B�BB�TB�BB�/B�#B��B��B��B�?B�B��B��B��B�{B�bB�uB�{B��B��B��B��B��B��B��B��B�B�!B��B��B��B�{B�\B�oB��B��B��B�B�B�!B�'B�XBĜBĜB��B��BBÖBƨBȴBȴBɺBÖB�?B�B�B�B�B�B��B��B��B��B��B�{B�hB�DB�%B�B� B�B�B�1B��B��B�B�jBBBBŢBȴBɺBɺB��B��B��B��B��B��B��B�B�B�B�;B�HB�TB�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	1B	
=B	VB	uB	�B	�B	�B	 �B	(�B	1'B	6FB	7LB	8RB	:^B	?}B	A�B	<jB	:^B	>wB	L�B	Q�B	T�B	VB	XB	[#B	\)B	]/B	aHB	e`B	gmB	l�B	m�B	m�B	m�B	m�B	q�B	s�B	t�B	u�B	x�B	z�B	{�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	~�B	z�B	y�B	�B	�=B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�RB	�RB	�LB	�RB	�RB	�RB	�dB	�dB	�jB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	B	B	��B	��B	��B	��B	B	ŢB	ŢB	ŢB	ĜB	B	B	�}B	B	ǮB	��B	ɺB	ȴB	ŢB	ŢB	ŢB	ŢB	ÖB	��B	�}B	��B	��B	��B	B	ÖB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�#B	�)B	�BB	�fB	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�mB	�fB	�`B	�fB	�mB	�mB	�fB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
1B
\B
�B
&�B
.B
5?B
8RB
?}B
D�B
K�B
P�B
W
B
[#B
_;B
dZB
iyB
n�B
r�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	B	:B	�B	�B	9\B	Z$B	w�B	�5B	��B	�ZB	�B	�"B	��B	��B	ĝB	�_B	�B	��B	�OB	��B	�RB	ţB
rB
aCB
t�B
~�B
��B
�VB
ŚB
�B
�KB
��B
��B �BBCBwB"�BI�B�"B��B%�BD�BS�BG�B0B-B%�B!�B	)BB �B��B�B�B�B�fB�/B�B�B��B��BĐB�B��B�B{�Bz�B�Bs�B69B:SBAB<aB.	B#�B
��B
�KB
��B
�}B
�0B
��B
faB
F�B
3.B
wB
B	��B	�B	��B	{�B	aIB	8SB	/B	'�B	$�B	"�B	&�B	�B	�B	B��B�qB�GB��BŧB��B�kB�QB�FB�GB�'B�B�	B�B�%B�B�B�B��B�B�B�JB�jB�VB�8B�B��B��B��B�B��B�HB�YB�DB�1B�#B�B��B��B�DB�B�B��B��B�B�jB�}B��B��B��B��B��B��B��B��B��B�B�$B��B��B��B��B�cB�vB��B��B��B�B�B�$B�*B�ZBğBĞB��B��BBÚBƪBȸBȶBɺBÙB�DB�B�B�B�B�B��B��B��B��B��B�B�nB�KB�)B�B�B�B�$B�5B��B��B� B�lBBBBţBȴBɺBɻB��B��B��B��B��B��B� B�B�B�B�<B�HB�VB�`B�eB�rB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	0B	
9B	UB	qB	�B	�B	�B	 �B	(�B	1#B	6BB	7HB	8QB	:ZB	?yB	A�B	<dB	:ZB	>sB	L�B	Q�B	T�B	U�B	XB	[ B	\#B	]*B	aBB	eZB	geB	l�B	m�B	m�B	m�B	m�B	q�B	s�B	t�B	u�B	x�B	z�B	{�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	~�B	z�B	y�B	�B	�9B	�ZB	�nB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�6B	�HB	�IB	�DB	�IB	�IB	�IB	�\B	�[B	�`B	�aB	�iB	�tB	�vB	�|B	�{B	�yB	�zB	�{B	�|B	��B	��B	B	B	��B	��B	��B	��B	B	ŚB	ŗB	ŗB	ĕB	B	B	�uB	B	ǥB	ʻB	ɴB	ȨB	ŘB	ŚB	ŚB	śB	ÌB	�{B	�sB	�zB	��B	��B	B	ËB	ŚB	ǦB	ɲB	ɲB	ʸB	��B	˾B	˽B	˾B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�7B	�[B	�oB	�sB	�vB	�|B	�B	�B	�}B	�qB	�oB	�hB	�bB	�[B	�WB	�]B	�dB	�eB	�]B	�VB	�\B	�[B	�bB	�jB	�mB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
(B
OB
�B
&�B
.	B
52B
8DB
?pB
D�B
K�B
P�B
V�B
[B
_.B
dNB
imB
n�B
r�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160404091702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160404091702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160404091702  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                