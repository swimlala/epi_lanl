CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-04T20:17:38Z AOML 3.0 creation; 2016-08-07T21:51:23Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151104201738  20160807145123  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               UA   AO  5287_9017_085                   2C  D   APEX                            6529                            072314                          846 @�|-��3�1   @�|.F)�@0��l�C��d��n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    UA   B   B   @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(33C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dx��D�3D�@ D��fD�ɚD�	�D�FfD�s3D�� D��D�<�D�y�D���D�	�D�I�D�<�D��3D�	�D�I�D�ffD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @O\)@��H@��HAp�A%p�AG
>Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��GB��GB�z�B��B��B��B��B��B��B��GB��GB̮BЮBԮB�z�BܮB�B�B�B��GB��GB��B��B��C W
CW
CW
CW
CW
C
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(�=C*=pC,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTp�CVp�CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D )D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ)DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�D�D�J�D��GD��{D�{D�QGD�~D���D��D�G�D��{D�׮D�{D�T{D�G�D��D�{D�T{D�qGD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A㗍A㗍A�hA�DA�PA�DA�\A�7A�\A㗍A㙚A��A��A��A�A�!A�!A�-A�9A�9A�FA�RA�jA�wA�wA���A�wA���A�ƨA��
A��#A��#A���A���A���A���A���A�A�jA�~�A�JA�C�A���A��TA׬Aח�A��A��AԴ9A�oA�C�A�A�A���A�jAΓuA�I�A�M�A��yA�&�A�$�A�jAȸRA�C�A�~�AĸRAå�A�ȴA�x�A��A�`BA���A�ƨA�%A���A��yA���A��;A�E�A�A�Q�A�?}A��
A�G�A�/A���A�ĜA��RA���A���A�{A��9A���A�5?A��A�\)A��A���A�\)A�\)A��
A�JA�A���A�/A�^5A~�jA}AzjAu��Ar�Aq��AkG�AeO�Ac
=A\�DAU��AQ�FAMp�AL5?AK�TAJ5?AH-AF��AC�AB�yA@��A>��A=�-A<$�A;�TA;A8ZA5�mA5/A4�HA3ƨA2ĜA1�hA0ffA/��A.��A.��A-�A-?}A,�HA+�
A*jA*bA)t�A)�A(��A'�^A&�`A&M�A%�7A$�A#��A#?}A"��A"5?A!�A!�A!l�A!�7A!�FA!�-A!��A!��A"1A!�#A!��A!t�A ��A ��A �A ĜAXA�\AVA$�AƨA"�A�9A��A�RA�!AA~�A�AC�A+A/A�AG�A�A%AI�A`BA�uAoA��A1'A��A7LAXA��At�A�A�A��A1'AoA�jA�+A�A"�A ȴA jA 1@�|�@�+@��y@�n�@��@�O�@���@�\)@�@��@���@�%@�S�@�7L@��9@�A�@��@��@�F@�w@�|�@�S�@�"�@��@�J@�bN@�\)@�o@�o@�+@�S�@���@��T@�j@�D@�^5@�G�@���@��@� �@�S�@�-@�V@�u@�z�@�@�j@䛦@��/@�Z@⟾@�@�M�@�X@܃@���@�\)@��H@�5?@��@ّh@ف@٩�@�/@��@��y@֏\@��@���@�x�@���@ӶF@�\)@�o@�E�@��T@Ѻ^@д9@ϝ�@�|�@�C�@·+@��T@ͩ�@�hs@�%@̓u@�ƨ@�ȴ@�@Ɂ@�?}@��@ț�@�I�@���@�l�@�@��@���@Ɨ�@ř�@��@��/@Ĭ@ģ�@ă@�z�@�r�@�1'@�t�@�^5@�{@���@�O�@��@��@�V@��/@�Ĝ@��j@��9@��u@�1'@�dZ@�33@�+@�+@�+@�"�@���@��^@�`B@��@��@�  @�\)@��+@�@��@�5?@���@���@�x�@�hs@�?}@�%@�Ĝ@���@�z�@�(�@�ƨ@���@��@�E�@��T@�@��@�`B@��`@��@��m@�dZ@�+@�o@���@��@��!@�^5@�x�@�&�@�V@�r�@� �@��@�b@��@��y@���@���@��!@�~�@�~�@��+@��\@���@��/@�z�@�Q�@�  @��;@��w@�S�@�o@���@��H@��\@�{@���@���@�9X@���@��@�K�@�"�@���@�E�@��@���@��@�p�@�7L@�V@���@�Ĝ@�Z@�(�@���@�l�@�33@�o@��@�~�@�^5@�V@�E�@�=q@��@�@��@�@��T@��h@�p�@�hs@�`B@�O�@���@�Z@���@�;d@��H@���@�~�@�ff@�$�@���@�x�@��@��@��`@��`@��`@��`@��/@���@�bN@��;@��@�\)@�
=@��H@��!@�~�@�n�@�^5@���@���@��u@�I�@�G�@��@��@�;d@x1'@o+@e�-@^�+@W
=@K�
@CdZ@;��@41@,�@(1'@"��@p�@��@S�@��@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A㗍A㗍A�hA�DA�PA�DA�\A�7A�\A㗍A㙚A��A��A��A�A�!A�!A�-A�9A�9A�FA�RA�jA�wA�wA���A�wA���A�ƨA��
A��#A��#A���A���A���A���A���A�A�jA�~�A�JA�C�A���A��TA׬Aח�A��A��AԴ9A�oA�C�A�A�A���A�jAΓuA�I�A�M�A��yA�&�A�$�A�jAȸRA�C�A�~�AĸRAå�A�ȴA�x�A��A�`BA���A�ƨA�%A���A��yA���A��;A�E�A�A�Q�A�?}A��
A�G�A�/A���A�ĜA��RA���A���A�{A��9A���A�5?A��A�\)A��A���A�\)A�\)A��
A�JA�A���A�/A�^5A~�jA}AzjAu��Ar�Aq��AkG�AeO�Ac
=A\�DAU��AQ�FAMp�AL5?AK�TAJ5?AH-AF��AC�AB�yA@��A>��A=�-A<$�A;�TA;A8ZA5�mA5/A4�HA3ƨA2ĜA1�hA0ffA/��A.��A.��A-�A-?}A,�HA+�
A*jA*bA)t�A)�A(��A'�^A&�`A&M�A%�7A$�A#��A#?}A"��A"5?A!�A!�A!l�A!�7A!�FA!�-A!��A!��A"1A!�#A!��A!t�A ��A ��A �A ĜAXA�\AVA$�AƨA"�A�9A��A�RA�!AA~�A�AC�A+A/A�AG�A�A%AI�A`BA�uAoA��A1'A��A7LAXA��At�A�A�A��A1'AoA�jA�+A�A"�A ȴA jA 1@�|�@�+@��y@�n�@��@�O�@���@�\)@�@��@���@�%@�S�@�7L@��9@�A�@��@��@�F@�w@�|�@�S�@�"�@��@�J@�bN@�\)@�o@�o@�+@�S�@���@��T@�j@�D@�^5@�G�@���@��@� �@�S�@�-@�V@�u@�z�@�@�j@䛦@��/@�Z@⟾@�@�M�@�X@܃@���@�\)@��H@�5?@��@ّh@ف@٩�@�/@��@��y@֏\@��@���@�x�@���@ӶF@�\)@�o@�E�@��T@Ѻ^@д9@ϝ�@�|�@�C�@·+@��T@ͩ�@�hs@�%@̓u@�ƨ@�ȴ@�@Ɂ@�?}@��@ț�@�I�@���@�l�@�@��@���@Ɨ�@ř�@��@��/@Ĭ@ģ�@ă@�z�@�r�@�1'@�t�@�^5@�{@���@�O�@��@��@�V@��/@�Ĝ@��j@��9@��u@�1'@�dZ@�33@�+@�+@�+@�"�@���@��^@�`B@��@��@�  @�\)@��+@�@��@�5?@���@���@�x�@�hs@�?}@�%@�Ĝ@���@�z�@�(�@�ƨ@���@��@�E�@��T@�@��@�`B@��`@��@��m@�dZ@�+@�o@���@��@��!@�^5@�x�@�&�@�V@�r�@� �@��@�b@��@��y@���@���@��!@�~�@�~�@��+@��\@���@��/@�z�@�Q�@�  @��;@��w@�S�@�o@���@��H@��\@�{@���@���@�9X@���@��@�K�@�"�@���@�E�@��@���@��@�p�@�7L@�V@���@�Ĝ@�Z@�(�@���@�l�@�33@�o@��@�~�@�^5@�V@�E�@�=q@��@�@��@�@��T@��h@�p�@�hs@�`B@�O�@���@�Z@���@�;d@��H@���@�~�@�ff@�$�@���@�x�@��@��@��`@��`@��`@��`@��/@���@�bN@��;@��@�\)@�
=@��H@��!@�~�@�n�@�^5@���@���@��uG�O�@�G�@��@��@�;d@x1'@o+@e�-@^�+@W
=@K�
@CdZ@;��@41@,�@(1'@"��@p�@��@S�@��@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
R�B
O�B
W
B
bNB
dZB
hsB
hsB
iyB
iyB
jB
jB
jB
iyB
bNB
YB
N�B
o�B
�1B
�oB
��B
�9B
��B
��B
��BB �BK�B�\B�3B��B�
B��B�B&�B-BL�BZBs�Bn�Bk�BbNBy�B�qB�}B�FB�wB�FB�wBƨBĜBǮBƨBB��B}�BgmBE�B%�B
=B��B��Bo�BZBM�B;dB+B�B\B  B
��B
�BB
�^B
��B
�PB
o�B
W
B
N�B
@�B
�B
oB
B	�sB	�B	��B	��B	�B	p�B	L�B	-B	�B	PB		7B	+B	B��B��B�B�B�B�B�B�B�B�yB�B�B�B�B�B�B��B��B��B	B	B	B	%B	%B	DB	uB	{B	�B	�B	�B	 �B	'�B	.B	33B	8RB	>wB	E�B	H�B	R�B	ZB	jB	k�B	|�B	�7B	��B	��B	�B	�FB	�dB	��B	ĜB	��B	�
B	�B	�B	�)B	�BB	�HB	�TB	�mB	�fB	�mB	�B	��B	��B	��B	�B	�mB	��B	�jB	�jB	ɺB	��B	�;B	��B	�LB	��B	�RB	��B	��B	�B	��B	�XB	�{B	{�B	v�B	t�B	z�B	u�B	jB	gmB	k�B	o�B	o�B	l�B	k�B	jB	iyB	iyB	iyB	hsB	hsB	hsB	l�B	q�B	w�B	z�B	� B	�%B	�%B	~�B	y�B	z�B	{�B	|�B	|�B	~�B	�=B	�PB	�VB	�\B	�hB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�LB	�RB	�wB	��B	�^B	�jB	�RB	�FB	�9B	�9B	�9B	�FB	�LB	�LB	�XB	�^B	�wB	�jB	�qB	�qB	�wB	�}B	�wB	�qB	�dB	�^B	�^B	�^B	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	�wB	�wB	��B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�BB	�NB	�NB	�TB	�TB	�TB	�NB	�NB	�HB	�BB	�BB	�BB	�BB	�BB	�;B	�;B	�HB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�sB	�mB	�mB	�mB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
	7B
	7B
DB
bB
�B
�B
�B
$�B
,B
33B
8RB
=qB
E�B
L�B
Q�B
YB
_;B
cTB
iyB
n�B
s�B
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
R�B
O�B
V�B
b;B
dIB
h^B
h_B
icB
ieB
jlB
jjB
jjB
igB
b9B
YB
N�B
o�B
�B
�ZB
��B
�#B
��B
��B
̵B B �BK�B�?B�BʤB��B��BzB&�B,�BL�BZ Bs�Bn~BkiBb/By�B�UB�bB�*B�ZB�,B�\BƏB�BǎBƌB�vB��B}�BgNBE�B%�B
B��B��Bo�BY�BM�B;EB*�B�B<B
��B
��B
�"B
�?B
��B
�5B
o~B
V�B
N�B
@gB
�B
QB
 �B	�ZB	��B	ˬB	��B	��B	p�B	L�B	,�B	�B	:B		#B	B	 �B��B��B�vB�kB�lB�lB�jB�vB�jB�eB�uB�B�B�B�B�B��B��B��B	�B	�B	B	B	B	,B	[B	cB	vB	|B	{B	 �B	'�B	-�B	3B	88B	>\B	E�B	H�B	R�B	ZB	jcB	kkB	|�B	�B	��B	��B	��B	�)B	�HB	�fB	�B	��B	��B	��B	��B	�B	�"B	�'B	�4B	�MB	�EB	�OB	�rB	��B	��B	��B	�yB	�MB	θB	�IB	�IB	ɛB	ϽB	�B	��B	�.B	��B	�3B	�fB	̯B	��B	��B	�8B	�^B	{�B	v�B	t�B	z�B	u�B	j`B	gNB	keB	o�B	o�B	lnB	kiB	jaB	i\B	i[B	i[B	hVB	hVB	hVB	lmB	q�B	w�B	z�B	�B	�B	�B	~�B	y�B	z�B	{�B	|�B	|�B	~�B	�B	�1B	�6B	�>B	�FB	�\B	�^B	�[B	�mB	�rB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�)B	�0B	�UB	�dB	�=B	�IB	�0B	�!B	�B	�B	�B	�"B	�)B	�+B	�7B	�;B	�TB	�IB	�MB	�OB	�VB	�[B	�WB	�OB	�EB	�<B	�:B	�<B	�IB	�NB	�RB	�TB	�UB	�YB	�UB	�[B	�XB	�YB	�ZB	�TB	�UB	�bB	�kB	�tB	�pB	�rB	�rB	�rB	�{B	�wB	�xB	�B	ƆB	ƅB	ƃB	ˤB	̫B	̫B	ͰB	ͰB	ͯB	ͱB	ϼB	��B	��B	��B	ϼB	ϸB	ϻB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�(B	�2B	�1B	�1B	�+B	�(B	�$B	�B	�B	�B	�B	�B	�B	�B	�$B	�:B	�BB	�BB	�GB	�RB	�VB	�UB	�VB	�OB	�HB	�HB	�LB	�@B	�AB	�IB	�NB	�PB	�UB	�SB	�XB	�VB	�VB	�VB	�UB	�SB	�VB	�VB	�VB	�UB	�[B	�YB	�UB	�XB	�YB	�YB	�ZB	�ZB	�_B	�bB	�`B	�cB	�eB	�eB	�gB	�nB	�hB	�fB	�mB	�jB	�mB	�oB	�pB	�rB	�yB	�zB	�zB	�yB	�yB	�yB	�sB	�sB	�vB	�zB	�xB	�{B	�xB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
 B
 B
B
	B
	G�O�B
;B
bB
oB
�B
$�B
+�B
3B
8+B
=LB
E{B
L�B
Q�B
X�B
_B
c+B
iQB
noB
s�B
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451232016080714512320160807145123  AO  ARCAADJP                                                                    20151104201738    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151104201738  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151104201738  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145123  IP                  G�O�G�O�G�O�                