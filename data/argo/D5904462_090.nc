CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-01T10:16:42Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151201101642  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  5287_9017_090                   2C  D   APEX                            6529                            072314                          846 @ׂķ.�P1   @ׂ�O���@0�$�/�d�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB��B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D��3D�FfD�vfD�ٚD�3D�P D��fD���D�3D�FfD��3D��fD� D�33Dڐ D��fD�  D�P D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A��A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB��B!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq��By��B�k�B���B���B���B���B���B���B���B���B�8RB�k�B�k�B���B���B���B���B���B���BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�=D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�qD��D�PRD��RD��D�D�Y�D��RD�ƹD�D�PRD��D��RD��D�=Dڙ�D��RD�	�D�Y�D�y�D�ƹ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA�JA�VA�JA�VA�
=A�JA�VA�oA�
=A�A��`A��`A��`A��`A��`A��`A��`A��`A��mA��A��A��A���A�VA�bA�1A��yAھwAڅA�oA�S�A���A��mA�VA�VA��yA�33A�%AҺ^A�t�A̮A�A��`AȾwA�l�A���A�$�Aź^A�?}AÅA�JA�&�A�JA���A�  A��jA�%A�A�A��uA��/A�G�A��jA�%A��A��;A��DA�/A�ȴA��A�JA�ȴA�ffA��FA�`BA��RA���A�5?A��wA��A�l�A�VA��A�33A��PA�7LA�7LA�|�A��HA���A�l�A��RA��+A��A���A��RA���A��A��+A���A�{A�VAO�Azz�Aw��Aw�At��Ap�AlĜAi&�AeO�Ac`BA_��A]�hA[�PAY��AWt�AU;dAR�HAPJAM�mAJ�+AHn�AG�PAFjADĜAC�AAoA?;dA>�A<��A;/A8�A8��A8�+A8bNA7��A7�A7VA5t�A3�A2r�A0�A0(�A/`BA.�HA.(�A-�#A-�A-��A+�^A)S�A(r�A';dA&��A%�A$�DA#��A"v�A!��A!%A��A�A\)A�A��A��AVA5?A$�A1Al�AȴA�PA�A�A�jA�\A1'AffA��A�DA�9A�+A��A�A�\A	�FA{A?}A�+A��A��Ax�A`BA��AhsA�A�-A5?AJA"�AVA�mA�A��A�PA��A�AS�A�HA�A�AjAE�A1'AO�A �HA ffA I�A I�A bN@��
@�
=@��@�$�@��@���@�ƨ@�X@���@�l�@�V@��u@�S�@��@�p�@�O�@�J@�E�@�{@���@�z�@��D@�@�r�@�A�@���@�A�@� �@�j@�1'@@�ȴ@�E�@홚@�%@��@�%@���@�v�@ꟾ@��@���@�`B@��D@�dZ@�@�x�@�hs@�dZ@�I�@��@ޟ�@�O�@��#@���@�ƨ@�5?@�1'@Ӯ@Ѻ^@�O�@�^5@�t�@��
@�o@љ�@�-@�t�@Ͳ-@�x�@�x�@�-@�$�@�p�@�I�@�\)@��@ʏ\@ɉ7@�V@��`@ȋD@�|�@�o@Ƈ+@�@���@� �@�\)@��H@�ff@���@���@��9@��j@���@�r�@� �@��w@�\)@��@�n�@���@�/@�j@���@�l�@�n�@��@��@��;@��@���@�"�@���@�~�@�5?@��@�{@��@���@���@�hs@��@��/@�bN@���@�|�@�K�@�@�ȴ@��!@��!@�ff@�hs@��`@�Z@�b@��@�l�@�o@���@���@�V@��@���@�G�@���@��u@��j@��@��@�9X@��@���@���@�"�@�ȴ@���@���@���@��9@�  @�;d@��@��R@�v�@���@�x�@�?}@�/@�%@�bN@��@���@�dZ@�dZ@�K�@�ȴ@�@���@��h@�/@��@��@��`@�%@��u@� �@��@�
=@���@���@�ȴ@�ȴ@���@�M�@���@��@�X@�?}@�/@��@���@��/@��9@�Q�@�ƨ@�C�@�33@�"�@���@��@��!@�v�@��@��#@��7@�G�@�&�@�%@��@�j@�9X@��m@��P@�S�@�"�@��R@�=q@�5?@�$�@��@�@���@��7@�`B@�%@��u@�Z@��@���@��F@��P@�t�@�dZ@�S�@�C�@�;d@��@���@�V@���@��7@�O�@���@���@���@�bN@���@���@��@�l�@�33@���@���@�  @���@���@yX@n�y@g|�@_l�@W
=@NV@F��@>ff@41@/�w@)�7@&@!�^@/@1'@S�@;d@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�JA�JA�VA�JA�VA�
=A�JA�VA�oA�
=A�A��`A��`A��`A��`A��`A��`A��`A��`A��mA��A��A��A���A�VA�bA�1A��yAھwAڅA�oA�S�A���A��mA�VA�VA��yA�33A�%AҺ^A�t�A̮A�A��`AȾwA�l�A���A�$�Aź^A�?}AÅA�JA�&�A�JA���A�  A��jA�%A�A�A��uA��/A�G�A��jA�%A��A��;A��DA�/A�ȴA��A�JA�ȴA�ffA��FA�`BA��RA���A�5?A��wA��A�l�A�VA��A�33A��PA�7LA�7LA�|�A��HA���A�l�A��RA��+A��A���A��RA���A��A��+A���A�{A�VAO�Azz�Aw��Aw�At��Ap�AlĜAi&�AeO�Ac`BA_��A]�hA[�PAY��AWt�AU;dAR�HAPJAM�mAJ�+AHn�AG�PAFjADĜAC�AAoA?;dA>�A<��A;/A8�A8��A8�+A8bNA7��A7�A7VA5t�A3�A2r�A0�A0(�A/`BA.�HA.(�A-�#A-�A-��A+�^A)S�A(r�A';dA&��A%�A$�DA#��A"v�A!��A!%A��A�A\)A�A��A��AVA5?A$�A1Al�AȴA�PA�A�A�jA�\A1'AffA��A�DA�9A�+A��A�A�\A	�FA{A?}A�+A��A��Ax�A`BA��AhsA�A�-A5?AJA"�AVA�mA�A��A�PA��A�AS�A�HA�A�AjAE�A1'AO�A �HA ffA I�A I�A bN@��
@�
=@��@�$�@��@���@�ƨ@�X@���@�l�@�V@��u@�S�@��@�p�@�O�@�J@�E�@�{@���@�z�@��D@�@�r�@�A�@���@�A�@� �@�j@�1'@@�ȴ@�E�@홚@�%@��@�%@���@�v�@ꟾ@��@���@�`B@��D@�dZ@�@�x�@�hs@�dZ@�I�@��@ޟ�@�O�@��#@���@�ƨ@�5?@�1'@Ӯ@Ѻ^@�O�@�^5@�t�@��
@�o@љ�@�-@�t�@Ͳ-@�x�@�x�@�-@�$�@�p�@�I�@�\)@��@ʏ\@ɉ7@�V@��`@ȋD@�|�@�o@Ƈ+@�@���@� �@�\)@��H@�ff@���@���@��9@��j@���@�r�@� �@��w@�\)@��@�n�@���@�/@�j@���@�l�@�n�@��@��@��;@��@���@�"�@���@�~�@�5?@��@�{@��@���@���@�hs@��@��/@�bN@���@�|�@�K�@�@�ȴ@��!@��!@�ff@�hs@��`@�Z@�b@��@�l�@�o@���@���@�V@��@���@�G�@���@��u@��j@��@��@�9X@��@���@���@�"�@�ȴ@���@���@���@��9@�  @�;d@��@��R@�v�@���@�x�@�?}@�/@�%@�bN@��@���@�dZ@�dZ@�K�@�ȴ@�@���@��h@�/@��@��@��`@�%@��u@� �@��@�
=@���@���@�ȴ@�ȴ@���@�M�@���@��@�X@�?}@�/@��@���@��/@��9@�Q�@�ƨ@�C�@�33@�"�@���@��@��!@�v�@��@��#@��7@�G�@�&�@�%@��@�j@�9X@��m@��P@�S�@�"�@��R@�=q@�5?@�$�@��@�@���@��7@�`B@�%@��u@�Z@��@���@��F@��P@�t�@�dZ@�S�@�C�@�;d@��@���@�V@���@��7@�O�@���@���@���@�bN@���@���@��@�l�@�33@���G�O�@�  @���@���@yX@n�y@g|�@_l�@W
=@NV@F��@>ff@41@/�w@)�7@&@!�^@/@1'@S�@;d@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	DB	DB	DB	DB	DB	JB	JB	JB	DB	JB	JB	VB	VB	VB	\B	\B	\B	\B	\B	bB	hB	uB	{B	�B	,B	1'B	49B	B�B	W
B	�B	��B
I�B
�B
��B
�mB
��BB5?BXBk�BG�BM�Bz�Bk�BjBy�B�B�B�BoB��BB#�B49BK�BZBbNBw�Bn�By�B�B�3B�XB�XB�?B��B|�BD�B7LB>wB49B$�B�BVB+B�B��B�wB�XB��B�oB�1B�7B�JBw�B_;BF�B7LB!�BoB  B
�fB
��B
�XB
�B
��B
�{B
}�B
m�B
]/B
33B
B	�BB	��B	�!B	��B	��B	�1B	p�B	ZB	B�B	2-B	 �B	�B	PB	+B��B��B�B�sB�`B�`B�mB�mB�sB�B�B�B�B��B��B	B	JB	hB	�B	�B	�B	�B	hB	DB	JB	DB	
=B		7B	VB	�B	�B	�B	 �B	 �B	�B	�B	�B	 �B	�B	$�B	�B	�B	�B	�B	!�B	"�B	 �B	 �B	�B	�B	�B	 �B	$�B	-B	33B	;dB	?}B	:^B	:^B	?}B	@�B	>wB	>wB	;dB	{B�sBȴB�!B�TB��B��B�fB��B��B�mB�ZB�mB�B�B��B	hB	�B	!�B	5?B	:^B	8RB	6FB	D�B	R�B	[#B	bNB	l�B	p�B	v�B	{�B	�%B	�=B	�JB	�PB	�oB	��B	�uB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�\B	�JB	�%B	�B	~�B	|�B	|�B	�B	�%B	�+B	�B	�%B	�7B	�JB	�JB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	�+B	�B	��B	�PB	�hB	��B	��B	�-B	��B	��B	��B	��B	��B	�hB	�VB	�VB	�PB	�JB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�'B	�-B	�-B	�9B	�XB	�dB	�qB	�wB	�wB	��B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ȴB	��B	ȴB	��B	��B	�)B	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
	7B

=B

=B

=B
	7B
	7B
	7B
DB
DB
DB
JB
JB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
#�B
+B
2-B
7LB
<jB
D�B
J�B
O�B
\)B
`BB
e`B
hsB
l�B
p�B
t�B
x�B
|�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	>B	@B	>B	@B	@B	DB	FB	DB	=B	DB	CB	QB	QB	NB	YB	WB	WB	WB	WB	_B	eB	pB	xB	�B	,B	1 B	42B	B�B	WB	�B	˺B
I�B
��B
;B
�VB
��BB5(BW�BknBG�BM�Bz�BkkBjhBy�B�BoB�BTB��B�B#�B4!BK�BZ Bb6Bw�Bn}By�B��B�B�>B�<B�&B��B|�BD~B71B>ZB4 B$�BeB;BB�{B͵B�YB�:B��B�QB�B�B�0Bw�B_BF�B7-B!�BTB
��B
�LB
˭B
�>B
� B
��B
�_B
}�B
mwB
]B
3B
B	�)B	�sB	�B	��B	��B	�B	p�B	ZB	B~B	2B	 �B	rB	@B	B��B��B�B�bB�OB�PB�]B�^B�bB�vB�B�B�B��B��B	 B	6B	SB	nB	sB	tB	mB	WB	0B	6B	/B	
*B		$B	DB	�B	�B	�B	 �B	 �B	�B	�B	�B	 �B	�B	$�B	�B	�B	�B	�B	!�B	"�B	 �B	 �B	�B	�B	�B	 �B	$�B	,�B	3B	;PB	?fB	:GB	:IB	?fB	@mB	>`B	>^B	;NB	eB�^BȢB�B�AB��B��B�QB˳B̺B�WB�DB�YB�pB��B��B	RB	�B	!�B	5'B	:FB	8;B	6-B	D�B	R�B	[	B	b5B	lrB	p�B	v�B	{�B	�
B	�#B	�/B	�5B	�UB	�cB	�XB	�KB	�SB	�ZB	��B	��B	��B	��B	��B	��B	��B	�~B	�dB	�LB	�?B	�.B	�B	��B	~�B	|�B	|�B	��B	�B	�B	��B	�
B	�B	�.B	�.B	�-B	�>B	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�B	�B	��B	�eB	�5B	�LB	��B	��B	�B	��B	��B	��B	��B	�pB	�MB	�8B	�8B	�4B	�-B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	�B	�B	�9B	�FB	�SB	�\B	�YB	�cB	�nB	�pB	�uB	�wB	�wB	�vB	�}B	ȑB	ʣB	ȔB	ʡB	��B	�
B	�(B	�(B	�&B	�'B	�,B	�4B	�4B	�8B	�AB	�@B	�KB	�TB	�]B	�\B	�]B	�`B	�cB	�fB	�sB	�oB	�oB	�mB	�dB	�^B	�]B	�fB	�jB	�kB	�qB	�pB	�qB	�pB	�qB	�jB	�eB	�eB	�sB	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�|B	�rB	�jB	�lB	�pB	�vB	�sB	�pB	�lB	�iB	�rB	�sB	�wB	�uB	�wB	�wB	�}B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
	B

B

B

B
	B
	B
	B
"B
"B
"B
'B
(B
.B
(B
/B
+B
.B
4B
2B
4B
4B
3B
:B
9B
@B
?B
BB
DB
EB
JB
MB
QB
RB
RB
YB
XB
`G�O�B
sB
kB
�B
#�B
*�B
2	B
7*B
<GB
DyB
J�B
O�B
\B
`B
e;B
hNB
lfB
p�B
t�B
x�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151201101642    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151201101642  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151201101642  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                