CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:28Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230828  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               -A   AO  4055_7112_045                   2C  D   APEX                            5374                            041511                          846 @֞	�?��1   @֞
�Bp@:)��l�D�cs"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    -A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dyy�D��D�I�D��3D�p D�3D�C3D�y�D���D�  D�FfD�vfD�� D��D�C3Dډ�D��3D�fD�S3D�l�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,�D,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDtۅDy��D� �D�M�D��\D�t)D�\D�G\D�}�D���D�)D�J�D�z�D��)D��D�G\Dڍ�D��\D�
�D�W\D�p�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�AļjA�{A�G�A²-A���A��wA�dZA��mA�x�A�Q�A��A�33A��/A���A�5?A���A�hsA��HA��9A��\A���A�ƨA�n�A��HA�&�A�Q�A��#A��A��wA��A�(�A���A�O�A��A��\A�&�A�%A��A��HA��A��FA�/A�t�A�+A��A��A�hsA�+A�oA�~�A��hA�7LA��#A�`BA��^A�$�A�A�jA�
=A���A��/A�33A��A���A�|�A�v�A�33A��A�
=A���A��HA�ȴA���A�
=A���A��^A�\)A��A��\A�JA��TA�x�A�ZA�  A�/A��TA�jA�^5A��A��wA��yA�XA���A���A�(�A�(�A���A���A��A~z�A}�A{��Ay�#AwS�At�`Ar1Ao�;AnI�AmG�Al9XAkoAi�
Ag��Af�Ae��Ae;dAc��AbjAa\)A`  A]�TA\��A\{A[��AZ�+AY��AX�HAV�AU+AT^5AS+APv�AN�AN5?AMp�AM/AL  AJ�/AI�AG%AE�mACXAA�-A@ZA?l�A>ffA>bA=��A=C�A<��A;��A;O�A;7LA:��A8��A8Q�A7�7A7oA5��A5;dA4�A4�A4-A3%A1l�A0�A/t�A.��A.��A.��A.��A-�mA,��A+?}A*�+A)%A'dZA&��A&z�A%�#A%dZA$z�A$M�A$-A#��A"bNA!�hA�;A��A  A��Ax�A�RA9XA��A��A�A�+A�;AO�A��A��An�A�A�7A%A�9AM�A��A�A-A?}A�mA��Av�A�#A33Av�A1'A��A+A
�/A
^5A	�An�A�A��A�#AK�A��A��A  A�RAA�Ax�A�@�+@�p�@��F@�v�@�$�@�&�@��@��/@�~�@�@@��@�Ĝ@�I�@�K�@�G�@�b@�!@�G�@��@㕁@�E�@�O�@ߝ�@�K�@ޏ\@�X@�bN@�v�@ّh@ى7@أ�@ج@�1@֗�@��@�K�@�/@��@�dZ@��@�x�@�b@ʧ�@ɩ�@�bN@�\)@Ɨ�@�V@�I�@�ȴ@���@��9@�9X@�S�@�=q@��@�  @�33@���@�O�@���@��@���@��@�`B@�bN@���@���@��7@��@�I�@��@��+@�7L@���@��D@��;@���@��\@�@�x�@��`@�1@�K�@���@��T@�`B@���@��P@�ȴ@���@�n�@���@��@�1'@��@�33@��@���@�$�@�@���@�x�@�O�@��@�Q�@���@�K�@��@���@���@�~�@�^5@�V@�J@��@�`B@�%@��D@�z�@�r�@�j@�Z@�1@�t�@�33@�@�ȴ@���@��!@���@�5?@���@��@��@�(�@���@�+@��R@��+@��@��7@�?}@���@�A�@�  @���@���@�C�@���@��#@��h@�O�@��`@�z�@�I�@� �@�  @��w@�l�@�33@�
=@��H@���@�-@��@��-@�p�@�%@���@�z�@�r�@�Z@�9X@��@��
@��w@��@���@�K�@��H@���@��R@���@��+@�v�@�V@��#@��h@�p�@�/@��@���@���@��@�Z@�A�@�1'@�1@�  @���@��F@�l�@�K�@�"�@�
=@���@�ȴ@��+@�v�@�5?@���@�hs@�X@�?}@�%@��/@��@��D@�(�@��@�1@�@�;@��@+@~�@~��@~@}�-@}�-@}�-@}��@}�h@|�@|�D@|(�@{�m@{ƨ@{dZ@{33@{@z��@z��@zM�@z�@y�#@yx�@yG�@x�u@x �@w�@uV@k�@c�F@[�@T��@N��@G\)@A��@;�F@4��@.ff@(�`@$(�@l�@S�@�w@1@�@��@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�AļjA�{A�G�A²-A���A��wA�dZA��mA�x�A�Q�A��A�33A��/A���A�5?A���A�hsA��HA��9A��\A���A�ƨA�n�A��HA�&�A�Q�A��#A��A��wA��A�(�A���A�O�A��A��\A�&�A�%A��A��HA��A��FA�/A�t�A�+A��A��A�hsA�+A�oA�~�A��hA�7LA��#A�`BA��^A�$�A�A�jA�
=A���A��/A�33A��A���A�|�A�v�A�33A��A�
=A���A��HA�ȴA���A�
=A���A��^A�\)A��A��\A�JA��TA�x�A�ZA�  A�/A��TA�jA�^5A��A��wA��yA�XA���A���A�(�A�(�A���A���A��A~z�A}�A{��Ay�#AwS�At�`Ar1Ao�;AnI�AmG�Al9XAkoAi�
Ag��Af�Ae��Ae;dAc��AbjAa\)A`  A]�TA\��A\{A[��AZ�+AY��AX�HAV�AU+AT^5AS+APv�AN�AN5?AMp�AM/AL  AJ�/AI�AG%AE�mACXAA�-A@ZA?l�A>ffA>bA=��A=C�A<��A;��A;O�A;7LA:��A8��A8Q�A7�7A7oA5��A5;dA4�A4�A4-A3%A1l�A0�A/t�A.��A.��A.��A.��A-�mA,��A+?}A*�+A)%A'dZA&��A&z�A%�#A%dZA$z�A$M�A$-A#��A"bNA!�hA�;A��A  A��Ax�A�RA9XA��A��A�A�+A�;AO�A��A��An�A�A�7A%A�9AM�A��A�A-A?}A�mA��Av�A�#A33Av�A1'A��A+A
�/A
^5A	�An�A�A��A�#AK�A��A��A  A�RAA�Ax�A�@�+@�p�@��F@�v�@�$�@�&�@��@��/@�~�@�@@��@�Ĝ@�I�@�K�@�G�@�b@�!@�G�@��@㕁@�E�@�O�@ߝ�@�K�@ޏ\@�X@�bN@�v�@ّh@ى7@أ�@ج@�1@֗�@��@�K�@�/@��@�dZ@��@�x�@�b@ʧ�@ɩ�@�bN@�\)@Ɨ�@�V@�I�@�ȴ@���@��9@�9X@�S�@�=q@��@�  @�33@���@�O�@���@��@���@��@�`B@�bN@���@���@��7@��@�I�@��@��+@�7L@���@��D@��;@���@��\@�@�x�@��`@�1@�K�@���@��T@�`B@���@��P@�ȴ@���@�n�@���@��@�1'@��@�33@��@���@�$�@�@���@�x�@�O�@��@�Q�@���@�K�@��@���@���@�~�@�^5@�V@�J@��@�`B@�%@��D@�z�@�r�@�j@�Z@�1@�t�@�33@�@�ȴ@���@��!@���@�5?@���@��@��@�(�@���@�+@��R@��+@��@��7@�?}@���@�A�@�  @���@���@�C�@���@��#@��h@�O�@��`@�z�@�I�@� �@�  @��w@�l�@�33@�
=@��H@���@�-@��@��-@�p�@�%@���@�z�@�r�@�Z@�9X@��@��
@��w@��@���@�K�@��H@���@��R@���@��+@�v�@�V@��#@��h@�p�@�/@��@���@���@��@�Z@�A�@�1'@�1@�  @���@��F@�l�@�K�@�"�@�
=@���@�ȴ@��+@�v�@�5?@���@�hs@�X@�?}@�%@��/@��@��D@�(�@��@�1@�@�;@��@+@~�@~��@~@}�-@}�-@}�-@}��@}�h@|�@|�D@|(�@{�m@{ƨ@{dZ@{33@{@z��@z��@zM�@z�@y�#@yx�@yG�@x�u@x �@w�@uV@k�@c�F@[�@T��@N��@G\)@A��@;�F@4��@.ff@(�`@$(�@l�@S�@�w@1@�@��@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�B�B|�B{�Bx�Bu�Bq�Bn�Bz�B�7B�1B�+B�B�%B�DB�oB�\B�B�B�B�B�Bz�By�B� B�%B�B{�B�B�PB�+B�B�B|�Bx�Bu�B�B�bB��B��B��B��B��B��B��B��B��B��B��B�bB�%B�B{�Bs�BgmB^5BXBR�BR�BL�B@�B49B33B7LB2-B �B�B�B{BhBVBDBB��B��B�FB_;BQ�BB�B1B�B��B��B|�BYBB�B6FB!�BDBB
�B
�sB
�B
�^B
�bB
}�B
s�B
e`B
J�B
5?B
,B
�B
hB
  B	�B	�B	ĜB	�XB	�'B	��B	��B	�{B	�=B	�B	|�B	v�B	m�B	cTB	[#B	Q�B	F�B	?}B	<jB	9XB	8RB	5?B	,B	�B	�B	uB	hB	1B	B	B	  B��B��B�B�yB�NB�/B��BĜB�jB�FB�RB�^B�^B�XB�dB�XB�RBÖB�}B�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�JB�%B�B� B~�B}�B}�B|�B{�Bz�Bx�Bv�Bt�Br�Bq�Bp�Bn�Bm�Bk�BiyBgmBe`BbNB_;B[#BVBQ�BN�BM�BJ�BH�BF�BD�BB�B>wB9XB7LB49B1'B.B,B)�B(�B'�B%�B#�B"�B�B�B�B�B�B�B�B�B�B�BuBhBbBVBPBPBVBVBVBJBDBPBPBJBPBDBhBhBoBoBbB\BoB�B�B�B�B�B�B�B�B�B�B�B�B�BuB{B�B�B�B�B�B�B�B!�B!�B"�B$�B%�B'�B(�B+B,B,B.B1'B33B49B6FB7LB9XB<jB=qB>wB@�BB�BF�BG�BH�BI�BL�BM�BP�BR�BT�BXBZB[#B_;BaHBbNBjBn�Bo�Bo�Bs�Bv�Bx�B{�B}�B~�B� B�B�B�B�%B�+B�7B�JB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�3B�?B�XB�jB�wBBŢBɺB��B��B��B��B�B�B�#B�)B�/B�BB�ZB�B�B�B��B��B��B��B��B��B	B	B	B	%B	1B	JB	\B	bB	oB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	$�B	&�B	,B	-B	-B	/B	/B	0!B	1'B	5?B	7LB	8RB	9XB	:^B	<jB	@�B	B�B	C�B	D�B	D�B	F�B	F�B	G�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	S�B	S�B	W
B	\)B	_;B	_;B	`BB	bNB	cTB	e`B	ffB	jB	k�B	l�B	l�B	m�B	m�B	q�B	u�B	w�B	z�B	}�B	~�B	~�B	~�B	~�B	�B	�B	�1B	�=B	�=B	�JB	�PB	�PB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	ɺB	�NB	��B
+B
uB
�B
)�B
2-B
:^B
C�B
J�B
P�B
W
B
\)B
`BB
dZB
hsB
m�B
t�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�$B�B� B|�B{�Bx�Bu�Bq�Bn�Bz�B�1B�*B�(B�B�B�<B�iB�XB�B�B�B�B��Bz�By�B�B�"B�B{�B�B�HB�$B�B�B|�Bx�Bu�B��B�YB��B��B��B��B��B��B��B��B��B�B�yB�ZB� B�B{�Bs�BgeB^,BXBR�BR�BL�B@yB4-B3-B7FB2$B �B�B�BpB]BKB7BB��B��B�;B_/BQ�BB�B B�BʴB��B|�BYBB�B68B!�B7BB
�B
�kB
�B
�WB
�YB
}�B
s�B
eWB
J�B
59B
,B
�B
cB	��B	�B	�B	ĘB	�TB	�$B	��B	��B	�zB	�:B	�B	|�B	v�B	m�B	cSB	[$B	Q�B	F�B	?B	<kB	9YB	8QB	5AB	,	B	�B	�B	zB	kB	3B	B	B	 B��B��B�B�~B�QB�5B��BĢB�qB�LB�VB�cB�dB�]B�iB�_B�VBÝB��B�MB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�iB�SB�-B�B�BB}�B}�B|�B{�Bz�Bx�Bv�Bt�Br�Bq�Bp�Bn�Bm�Bk�BiBgxBeeBbUB_DB[+BVBQ�BN�BM�BJ�BH�BF�BD�BB�B>�B9GB7TB4CB12B.B,B*B(�B'�B%�B#�B"�B�B�B�B�B�B�B�B|B�BoBeBWBTBEB@B>BDBHBGB:BNB>B@B8B@BMBWBXB^BvBRBfBxB{B�B�B�B�BvB}B�B�B�B�B�B�B~B�B�B�B�B�B�B�B�B!�B!�B"�B$�B%�B'�B(�B+B,B,B.B1.B3<B4@B6NB7SB9`B<qB=yB>}B@�BB�BF�BG�BH�BI�BL�BM�BP�BR�BUBXBZ!B[*B_?BaLBbTBj�Bn�Bo�Bo�Bs�Bv�Bx�B{�B}�B~�B�B�B�B�B�&B�.B�;B�LB�eB�qB�xB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�B�2B�?B�XB�lB�xBBŢBɹB��B��B��B��B�B�B�"B�(B�.B�@B�XB�B�B�B��B��B��B��B��B��B	B	B	B	!B	.B	FB	[B	]B	kB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	$�B	&�B	,B	-	B	-B	/B	/B	0B	1"B	5<B	7HB	8KB	9QB	:ZB	<eB	@B	B�B	C�B	D�B	D�B	F�B	F�B	G�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	S�B	S�B	WB	\!B	_4B	_5B	`;B	bHB	cMB	eXB	f]B	jxB	kB	l�B	l�B	m�B	m�B	q�B	u�B	w�B	z�B	}�B	~�B	~�B	~�B	~�B	�B	�B	�*B	�6B	�8B	�CB	�IB	�IB	�SB	�\B	�fB	�oB	�tB	�B	��B	��B	��B	��B	��B	ɲB	�DB	��B
"B
jB
�B
)�B
2!B
:SB
C�B
J�B
P�B
V�B
\B
`5B
dKB
hfB
m�B
t�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230828  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                