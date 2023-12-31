CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:48Z AOML 3.0 creation; 2016-08-07T21:17:30Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221248  20160807141730  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_011                   2C  D   APEX                            6487                            072314                          846 @�����1   @��8��@-XbM���c�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBI33BO��BX  B`  Bh  Bp  Bx  B�  B�33B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D��D�I�D���D���D�	�D�9�D���D�p D��D�VfD���D��3D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB��BKp�BQ�BZ=qBb=qBj=qBr=qBz=qB��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB��CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Dy��D�+�D�[�D���D�޹D��D�K�D���D���D�+�D�hRD���D��D�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A۾wA۾wA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA۶FA۩�Aۣ�Aۡ�A۲-AۼjA۸RA۾wA۴9Aۙ�Aە�AۑhA�bA�I�A�$�AґhA�l�A�l�AΗ�A���A�ĜA�|�A�%A�A�A�oA�A��
A��TA���A��RA�+A��yA���A��jA�"�A���A�+A��A���A�r�A��mA��A��9A��A�A�A��9A��A��^A��+A��A�/A���A�"�A�bA���A�\)A��A���A�ĜA�G�A��RA��A��A��A��hA�v�A�O�A�-A��A���A�
=A�n�A���A���A�K�A~jA|Q�Aw;dAq�Al{Ah9XAc+A_��A]�-A[l�AW?}ARJAO��ANĜALI�AE
=AC�hAB  A@~�A?"�A;��A9��A8�A89XA7�A6�`A6Q�A4��A3"�A0z�A.ffA-�TA+�#A)�hA(JA&�\A&E�A%�;A&1A'�;A(z�A(jA(v�A'?}A'oA'dZA'S�A&ȴA%��A#�A"��A"�DA"$�A!A!\)A!�A!��A"(�A"E�A"�jA#�A#&�A#"�A"�A"n�A!ƨA!p�A!G�A!33A ��A�uAp�A��A��AjA�A�jA=qA�
Ax�A�AI�A(�A�A�^AO�A�AĜA��A5?A�A�/AĜA��AjAbA��AVA�yAbA�hA�AjA�AƨAl�A33A��A�AA�A�A�mAS�A�A�+A �AƨAl�A
��A
n�A
{A	�wA	�A	&�A��AffA�jA��A�hAG�A�jA�AbA��A/A��A(�A�TA�7A7LA�A ��A r�A =qA �A @��w@���@�5?@�X@��@��@��@�j@�I�@�b@��F@�33@��R@��^@��-@���@�x�@�x�@��/@�Ĝ@�b@�l�@�v�@�b@�"�@�-@�u@�b@��m@띲@��y@�v�@��T@�-@�Q�@��@�7@�/@�Ĝ@�I�@�  @�1@�w@�
=@�P@�P@��@��@�&�@��D@�  @ߕ�@߶F@�\)@ް!@�&�@� �@�l�@�v�@��@�p�@�V@��
@�K�@�M�@���@Չ7@��@ԋD@ӝ�@�+@�v�@�{@љ�@�G�@�V@ЋD@�dZ@�M�@���@͙�@�7L@���@̼j@�bN@�9X@�  @��;@�\)@�V@��@�/@�z�@�b@�ƨ@ǍP@�S�@�"�@�
=@��@Ɵ�@Ƈ+@�V@�$�@��@őh@�/@Ĭ@�b@�t�@�
=@�@�^5@�=q@�@���@�X@�O�@��@��D@���@�o@��y@��@�x�@�p�@�&�@��@� �@�ƨ@���@�C�@�
=@���@�ff@�=q@��-@�hs@�%@���@�9X@�b@��F@�+@�V@�@��h@���@��j@��@���@�l�@�33@��y@���@�^5@��@�7L@�V@�Ĝ@�A�@��@���@��P@�S�@��@��y@���@�-@��@�&�@�Ĝ@�r�@�I�@�1'@�1@��@�@��y@���@�M�@��@�/@���@�I�@�(�@�b@��@�S�@��@�v�@�^5@�E�@���@���@���@��@�Q�@��@�o@��!@�~�@�$�@�@�O�@���@���@���@�bN@�9X@�ƨ@�|�@�
=@���@�n�@�-@��T@�@��7@�7L@���@�I�@�9X@�(�@�b@��
@�l�@�+@���@�=q@�J@��#@�@��^@��-@�O�@��@�9X@�  @��;@�t�@�@�ȴ@�V@�@�@�@��7@�V@��@���@�Z@�  @��
@��@�S�@��-@�ȴ@�G�@~��@xA�@r�@lj@e/@Y��@Pb@GK�@?�w@97L@3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A۾wA۾wA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA۶FA۩�Aۣ�Aۡ�A۲-AۼjA۸RA۾wA۴9Aۙ�Aە�AۑhA�bA�I�A�$�AґhA�l�A�l�AΗ�A���A�ĜA�|�A�%A�A�A�oA�A��
A��TA���A��RA�+A��yA���A��jA�"�A���A�+A��A���A�r�A��mA��A��9A��A�A�A��9A��A��^A��+A��A�/A���A�"�A�bA���A�\)A��A���A�ĜA�G�A��RA��A��A��A��hA�v�A�O�A�-A��A���A�
=A�n�A���A���A�K�A~jA|Q�Aw;dAq�Al{Ah9XAc+A_��A]�-A[l�AW?}ARJAO��ANĜALI�AE
=AC�hAB  A@~�A?"�A;��A9��A8�A89XA7�A6�`A6Q�A4��A3"�A0z�A.ffA-�TA+�#A)�hA(JA&�\A&E�A%�;A&1A'�;A(z�A(jA(v�A'?}A'oA'dZA'S�A&ȴA%��A#�A"��A"�DA"$�A!A!\)A!�A!��A"(�A"E�A"�jA#�A#&�A#"�A"�A"n�A!ƨA!p�A!G�A!33A ��A�uAp�A��A��AjA�A�jA=qA�
Ax�A�AI�A(�A�A�^AO�A�AĜA��A5?A�A�/AĜA��AjAbA��AVA�yAbA�hA�AjA�AƨAl�A33A��A�AA�A�A�mAS�A�A�+A �AƨAl�A
��A
n�A
{A	�wA	�A	&�A��AffA�jA��A�hAG�A�jA�AbA��A/A��A(�A�TA�7A7LA�A ��A r�A =qA �A @��w@���@�5?@�X@��@��@��@�j@�I�@�b@��F@�33@��R@��^@��-@���@�x�@�x�@��/@�Ĝ@�b@�l�@�v�@�b@�"�@�-@�u@�b@��m@띲@��y@�v�@��T@�-@�Q�@��@�7@�/@�Ĝ@�I�@�  @�1@�w@�
=@�P@�P@��@��@�&�@��D@�  @ߕ�@߶F@�\)@ް!@�&�@� �@�l�@�v�@��@�p�@�V@��
@�K�@�M�@���@Չ7@��@ԋD@ӝ�@�+@�v�@�{@љ�@�G�@�V@ЋD@�dZ@�M�@���@͙�@�7L@���@̼j@�bN@�9X@�  @��;@�\)@�V@��@�/@�z�@�b@�ƨ@ǍP@�S�@�"�@�
=@��@Ɵ�@Ƈ+@�V@�$�@��@őh@�/@Ĭ@�b@�t�@�
=@�@�^5@�=q@�@���@�X@�O�@��@��D@���@�o@��y@��@�x�@�p�@�&�@��@� �@�ƨ@���@�C�@�
=@���@�ff@�=q@��-@�hs@�%@���@�9X@�b@��F@�+@�V@�@��h@���@��j@��@���@�l�@�33@��y@���@�^5@��@�7L@�V@�Ĝ@�A�@��@���@��P@�S�@��@��y@���@�-@��@�&�@�Ĝ@�r�@�I�@�1'@�1@��@�@��y@���@�M�@��@�/@���@�I�@�(�@�b@��@�S�@��@�v�@�^5@�E�@���@���@���@��@�Q�@��@�o@��!@�~�@�$�@�@�O�@���@���@���@�bN@�9X@�ƨ@�|�@�
=@���@�n�@�-@��T@�@��7@�7L@���@�I�@�9X@�(�@�b@��
@�l�@�+@���@�=q@�J@��#@�@��^@��-@�O�@��@�9X@�  @��;@�t�@�@�ȴ@�V@�@�@�@��7@�V@��@���@�Z@�  @��
@��G�O�@��-@�ȴ@�G�@~��@xA�@r�@lj@e/@Y��@Pb@GK�@?�w@97L@3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBz�Bz�Bz�B{�Bz�Bz�Bz�B{�B{�B{�B{�By�By�Bx�Bx�By�By�By�By�Bx�Bw�Bw�Bv�Br�B�RB�)B�HB�BɺBǮBÖB��B��B[#BiyBdZB�B��B�Bw�B�1B��B��B�!B�/B�HB�B�PB��B��B��B��B��B��B�hB�B��B��BǮB�?BcTB%�B�B6FBhsBp�BgmBL�B,B�B��B��Bv�Bn�BcTBN�B:^B�B
��B
�RB
�hB
iyB
N�B
/B
oB	�B	�ZB	�sB	��B	��B	�%B	k�B	L�B	5?B	&�B	�B	B�B�TB�)B��B��B��B��BɺBƨBȴBȴBȴBǮBƨBǮB��B��B��BȴB�wB��B�^B�3B�3B�RB�wBĜB�/B	�B	0!B	33B	7LB	@�B	W
B	l�B	q�B	t�B	v�B	u�B	�B	�VB	��B	��B	�B	ɺB	�B	�B	��B
	7B
�B
�B
�B
 �B
+B
,B
+B
)�B
+B
+B
�B
DB
+B
	7B
DB

=B

=B
DB
DB
PB
bB
\B
\B
bB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
oB
{B
�B
�B
�B
�B
�B
{B
uB
oB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
DB
+B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B
  B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�yB	�fB	�mB	�`B	�ZB	�`B	�`B	�`B	�mB	�sB	�B	�B	�fB	�ZB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�B	�B	�B	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	�yB	�yB	�yB	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
	7B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
(�B
1'B
8RB
;dB
>wB
E�B
H�B
O�B
T�B
ZB
^5B
dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  Bz�Bz�Bz�B{�Bz�Bz�Bz�B{�B{�B{�B{�By�By�Bx�Bx�By�By�By�By�Bx�Bw�Bw�Bv�Br�B�$B��B�B��BɋBǃB�iB�YB��BZ�BiKBd*B��B��B��Bw�B��B�tB��B��B��B�B��B�B�fB�{B��B��B�{B�dB�5B��BͥBʒB�}B�Bc B%�B~B6Bh?BpoBg:BL�B+�B�{BвB�vBv�BncBcBN�B:,B�B
��B
�B
�5B
iDB
N�B
.�B
<B	�}B	�)B	�BB	��B	��B	��B	kXB	L�B	5B	&�B	fB	�B�_B�+B��BкBέB̢B˞BɏB�}BȈBȋBȋBǁB�zBǂBͦBϲB��BȈB�KB�VB�2B�B�B�#B�HB�oB�B	[B	/�B	3 B	7B	@QB	V�B	lVB	quB	t�B	v�B	u�B	��B	� B	�VB	�uB	��B	ɃB	��B	�KB	��B
�B
JB
`B
XB
 �B
*�B
+�B
*�B
)�B
*�B
*�B
RB
	B
�B
�B
	B

B

B
	B
	B
B
&B
"B
!B
&B
%B
1B
?B
pB
kB
WB
]B
]B
VB
VB
LB
EB
]B
]B
_B
JB
?B
2B
2B
>B
JB
KB
OB
KB
BB
=B
6B
1B
>B
=B
>B
=B
=B
=B
LB
BB
CB
PB
OB
OB
NB
IB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�cB	�AB	�TB	�RB	�`B	�eB	�bB	�\B	�[B	�ZB	�jB	�xB	�}B	�{B	�pB	�lB	�eB	�<B	�*B	�0B	�#B	�B	�"B	�!B	� B	�0B	�4B	�^B	�OB	�)B	�B	�B	�B	�B	�B	�#B	�$B	�"B	�AB	�eB	�`B	�PB	�GB	�?B	�5B	�5B	�@B	�MB	�MB	�>B	�<B	�:B	�9B	�?B	�;B	�5B	�GB	�MB	�MB	�XB	�YB	�XB	�SB	�IB	�QB	�XB	�XB	�YB	�_B	�ZB	�YB	�LB	�FB	�EB	�GB	�TB	�XB	�dB	�lB	�oB	�wB	�vB	�vB	�pB	�cB	�_B	�]B	�[B	�]B	�_B	�`B	�]B	�^B	�cB	�eB	�dB	�eB	�cB	�cB	�mB	�gB	�qB	�xB	�uB	�wB	�tB	�uB	�xB	�yB	�{B	�zB	�zB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
B
B
B
B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
!B
 B
!B
!B
%B
%B
$B
"B
%B
-B
+B
+B
-B
3B
2B
5B
4B
7B
8B
7B
7B
7B
8B
<B
=B
CB
FB
CB
KB
HB
IB
DB
EB
KB
KB
QB
SB
QB
PB
QB
PB
OB
WB
\B
]B
\B
]B
ZB
[B
ZB
dB
\G�O�B
iB
"�B
(�B
0�B
8B
;"B
>2B
E_B
HqB
O�B
T�B
Y�B
]�B
d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417302016080714173020160807141730  AO  ARCAADJP                                                                    20150226221248    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221248  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141730  IP                  G�O�G�O�G�O�                