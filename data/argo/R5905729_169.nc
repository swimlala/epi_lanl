CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-12-12T10:01:11Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߜ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20221212100111  20221212100111  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�ֻ�R�1   @��K�l@)
=p���d���$�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @@  @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B��B  B   B(  B0  B8ffB@ffBH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @>�R@��\@�\)@�(�A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��A��
B�B�B�B�B'�B/�B8Q�B@Q�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B�(�B��]B�\)B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�,)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֡�A֣�A֣�A֣�A֣�A֥�A֥�A֧�A֧�A֩�A֩�A֩�A֮Aֲ-Aִ9Aֲ-Aֲ-Aֺ^AָRA�ĜA�A�ȴA�ƨA���A���A��yA�ffAׅA�r�A�dZA�ZA�|�A�l�A�oAԼjA�9XA��AȶFA���A�S�A���A�S�A��TA��A�A�A�ȴA���A��A��\A�XA��7A���A��A���A��A��9A���A�ȴA���A�jA�9XA��A��RA���A� �A�A���A�A�A�-A��A��#A��\A���A�ȴA��A���A��yA��PA���A�
=A���A��jA���A{hsAw�At��AnĜAj�!Ah1Ad�+Aa��A`M�A]�PAW�AQ"�AKK�AI|�AH��AG��AE��AD1'ABA�AAoA?�A?�A>  A;�A:E�A7/A3�#A2ZA0��A/%A,1A*r�A)VA&��A%��A$�DA#A"�/A"(�A!��A!
=A"�A��AdZA��A�A��A�\Ap�AVA�AA�RA1'A�A�A�7A�PA�DAƨA�A��A��AE�A��A��AVA��A��A5?AE�Ap�A�DA��AoA�`A1'At�A
�DA	?}AZA��A��A	�A
�A	�;A	l�A	A	�wA	A	�wA	�
A	�A	��A	�A	AbAt�A�hA�wA�AS�A�`A��A^5A(�AhsA;dA+A��A�\A��A�jA��AI�At�A"�Av�AbNAQ�AbNAVA�A�mA�#AXA �A 9X@�E�@���@���@�(�@���@�;d@���@�X@� �@�l�@�ȴ@��@��@�@�O�@�A�@�;d@�x�@웦@�w@�t�@��H@�v�@���@�&�@�z�@��@���@�hs@��@�(�@�P@�"�@���@��@��@�h@�@�?}@�@�z�@��@�;d@�@޸R@ޏ\@�-@��#@ݙ�@�?}@ܛ�@�S�@ڟ�@�n�@�n�@�5?@��T@ى7@���@ج@�r�@���@ׅ@֗�@Ցh@��`@�j@��@�b@��
@�;d@ҏ\@�5?@�X@ЋD@�A�@϶F@Ο�@�@��T@��@�r�@˅@�-@���@ȃ@� �@�t�@�t�@�K�@�=q@�x�@�%@�Z@��;@�+@�ȴ@�V@�J@��#@��j@�1@�dZ@�"�@�ȴ@�^5@�J@��#@��h@��7@��h@�x�@�p�@�X@�O�@���@�z�@�b@��@�@���@�v�@�J@�@��@�?}@��@���@��`@���@�A�@�|�@�o@��@���@��@���@��@��@��j@�Z@��F@�|�@�+@�v�@��#@���@�hs@�&�@���@���@�9X@���@�\)@�S�@�K�@��y@���@�n�@�E�@��@�x�@�?}@�V@���@��F@���@��@�l�@�S�@�
=@���@�^5@�J@���@�x�@���@�j@���@���@�t�@�t�@�\)@�S�@�33@���@��\@��#@��@�&�@��@�1'@�b@���@���@��H@�v�@��@��7@�/@���@��w@��@��!@�E�@���@���@��j@�z�@�1'@�1@�1@�  @��P@��@��@��H@��H@��H@���@�ff@��@��-@�?}@�V@�Ĝ@���@��@��;@��@�;d@��y@��+@��@�x�@�/@�V@��9@�r�@� �@��
@��@�@��!@���@�ff@���@�@�?}@��`@�Ĝ@��D@�A�@���@���@�t�@�@���@�$�@�x�@�&�@�%@���@��u@�Z@�A�@��@�ƨ@�\)@��@���@��@�x�@���@�Q�@���@�"�@���@�^5@�E�@�$�@�@��#@�@��-@���@�?}@���@��/@���@���@�Ĝ@��9@�r�@�1'@���@��@��;@��;@�ƨ@��@���@�v�@�J@��-@���@��h@��7@�x�@�X@�/@��`@�z�@�  @|�@+@~�@~ff@}O�@}/@}�@}�@}V@|��@|�/@{��@{dZ@{@z�!@z-@y��@yx�@x��@x�@x1'@w�@wK�@w+@w�@w�@w
=@w�@vȴ@u��@t�@r��@rJ@q�^@q&�@p�9@pQ�@o�w@o;d@o
=@n�R@nff@m�-@m/@l�/@l�j@l��@lz�@kƨ@kS�@j��@j~�@j-@i��@ihs@iG�@i&�@i�@i�@h�`@h�u@hA�@g��@g��@g;d@f�+@f$�@e�T@e�@e/@d�@d�/@d�j@d��@dz�@dj@dj@dj@dZ@d(�@c�F@c"�@co@b�@b�@b�@b�@c@b�@bJ@ax�@a&�@`�`@`Q�@_�@^��@^�y@^ȴ@^��@^$�@]`B@]�@\�/@\Z@\(�@[��@[@Z��@Z-@Yx�@Y7L@Y�@Y%@X��@XĜ@Xr�@Xb@W�w@W|�@V�@VE�@U@U�-@U`B@T��@Tz�@T(�@S�m@S�@S@R-@R�@R�@Q��@P�@O�w@OK�@O�@Nv�@M��@M?}@L��@L(�@K�
@K�F@KC�@Jn�@J=q@J�@I��@I��@I�@HQ�@HQ�@HQ�@HA�@G�w@G
=@F��@Fv�@F5?@F{@E�T@E��@E�@D��@Dj@D9X@D�@C�m@C"�@B�H@B~�@A��@A��@A�@A�#@A�^@A�7@Ax�@AX@@��@@A�@?�@?�@>��@>��@>5?@=��@=p�@<�j@<�D@<1@;��@;S�@:��@:�\@:n�@:-@9�@9�#@9�#@9�^@9�7@97L@8Ĝ@8r�@7�;@7�@7��@7�P@6��@6��@6ff@6V@6V@65?@5��@5�@5O�@5V@4�j@4�D@4Z@49X@4�@3�
@3dZ@2�H@2�!@2�\@2M�@2J@1��@0��@0��@0��@0��@0�u@0Q�@0 �@/�@/+@.ff@-��@-�h@-�h@-�h@-�h@-p�@-`B@-?}@,�@,z�@,I�@,�@+�
@+��@+��@+dZ@*�!@*M�@)��@)X@)&�@)�@(�`@(��@(Q�@'�@'|�@'\)@'�@'�@'
=@'
=@&�y@&��@&��@&��@&�+@&v�@&ff@&5?@%@%�-@%��@%?}@$��@$��@$Z@#�F@#dZ@#o@"�H@"^5@"�@!�#@!�@ Ĝ@ �9@ ��@ �u@ �@ 1'@��@��@\)@\)@\)@\)@\)@K�@
=@v�@V@V@E�@E�@E�@5?@$�@@�T@p�@V@Z@�m@�
@�
@ƨ@��@S�@"�@o@�H@�!@��@M�@�#@�7@x�@G�@�`@��@�@Q�@b@��@�@l�@K�@
=@�@��@E�@@�h@�j@z�@j@Z@�@ƨ@�@t�@dZ@33@�H@~�@n�@^5@=q@�@��@�#@��@�7@G�@&�@��@�@r�@Q�@1'@b@b@b@�@�w@|�@;d@+@�@�@ȴ@�R@ȴ@ȴ@�R@V@�T@@��@��@��@��@�@�@V@V@V@��@�@��@�@j@9X@1@��@ƨ@�F@�@S�@C�@@
�!@
��@
��@
��@
��@
��@
��@
�\@
^5@
M�@
M�@
�@	�#@	��@	��@	��@	�^@	�7@	�7@	x�@	x�@	hs@	hs@	hs@	X@	&�@Ĝ@�@Q�@b@  @�@�@�;@�w@�P@\)@K�@K�@K�@K�@K�111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A֡�A֣�A֣�A֣�A֣�A֥�A֥�A֧�A֧�A֩�A֩�A֩�A֮Aֲ-Aִ9Aֲ-Aֲ-Aֺ^AָRA�ĜA�A�ȴA�ƨA���A���A��yA�ffAׅA�r�A�dZA�ZA�|�A�l�A�oAԼjA�9XA��AȶFA���A�S�A���A�S�A��TA��A�A�A�ȴA���A��A��\A�XA��7A���A��A���A��A��9A���A�ȴA���A�jA�9XA��A��RA���A� �A�A���A�A�A�-A��A��#A��\A���A�ȴA��A���A��yA��PA���A�
=A���A��jA���A{hsAw�At��AnĜAj�!Ah1Ad�+Aa��A`M�A]�PAW�AQ"�AKK�AI|�AH��AG��AE��AD1'ABA�AAoA?�A?�A>  A;�A:E�A7/A3�#A2ZA0��A/%A,1A*r�A)VA&��A%��A$�DA#A"�/A"(�A!��A!
=A"�A��AdZA��A�A��A�\Ap�AVA�AA�RA1'A�A�A�7A�PA�DAƨA�A��A��AE�A��A��AVA��A��A5?AE�Ap�A�DA��AoA�`A1'At�A
�DA	?}AZA��A��A	�A
�A	�;A	l�A	A	�wA	A	�wA	�
A	�A	��A	�A	AbAt�A�hA�wA�AS�A�`A��A^5A(�AhsA;dA+A��A�\A��A�jA��AI�At�A"�Av�AbNAQ�AbNAVA�A�mA�#AXA �A 9X@�E�@���@���@�(�@���@�;d@���@�X@� �@�l�@�ȴ@��@��@�@�O�@�A�@�;d@�x�@웦@�w@�t�@��H@�v�@���@�&�@�z�@��@���@�hs@��@�(�@�P@�"�@���@��@��@�h@�@�?}@�@�z�@��@�;d@�@޸R@ޏ\@�-@��#@ݙ�@�?}@ܛ�@�S�@ڟ�@�n�@�n�@�5?@��T@ى7@���@ج@�r�@���@ׅ@֗�@Ցh@��`@�j@��@�b@��
@�;d@ҏ\@�5?@�X@ЋD@�A�@϶F@Ο�@�@��T@��@�r�@˅@�-@���@ȃ@� �@�t�@�t�@�K�@�=q@�x�@�%@�Z@��;@�+@�ȴ@�V@�J@��#@��j@�1@�dZ@�"�@�ȴ@�^5@�J@��#@��h@��7@��h@�x�@�p�@�X@�O�@���@�z�@�b@��@�@���@�v�@�J@�@��@�?}@��@���@��`@���@�A�@�|�@�o@��@���@��@���@��@��@��j@�Z@��F@�|�@�+@�v�@��#@���@�hs@�&�@���@���@�9X@���@�\)@�S�@�K�@��y@���@�n�@�E�@��@�x�@�?}@�V@���@��F@���@��@�l�@�S�@�
=@���@�^5@�J@���@�x�@���@�j@���@���@�t�@�t�@�\)@�S�@�33@���@��\@��#@��@�&�@��@�1'@�b@���@���@��H@�v�@��@��7@�/@���@��w@��@��!@�E�@���@���@��j@�z�@�1'@�1@�1@�  @��P@��@��@��H@��H@��H@���@�ff@��@��-@�?}@�V@�Ĝ@���@��@��;@��@�;d@��y@��+@��@�x�@�/@�V@��9@�r�@� �@��
@��@�@��!@���@�ff@���@�@�?}@��`@�Ĝ@��D@�A�@���@���@�t�@�@���@�$�@�x�@�&�@�%@���@��u@�Z@�A�@��@�ƨ@�\)@��@���@��@�x�@���@�Q�@���@�"�@���@�^5@�E�@�$�@�@��#@�@��-@���@�?}@���@��/@���@���@�Ĝ@��9@�r�@�1'@���@��@��;@��;@�ƨ@��@���@�v�@�J@��-@���@��h@��7@�x�@�X@�/@��`@�z�@�  @|�@+@~�@~ff@}O�@}/@}�@}�@}V@|��@|�/@{��@{dZ@{@z�!@z-@y��@yx�@x��@x�@x1'@w�@wK�@w+@w�@w�@w
=@w�@vȴ@u��@t�@r��@rJ@q�^@q&�@p�9@pQ�@o�w@o;d@o
=@n�R@nff@m�-@m/@l�/@l�j@l��@lz�@kƨ@kS�@j��@j~�@j-@i��@ihs@iG�@i&�@i�@i�@h�`@h�u@hA�@g��@g��@g;d@f�+@f$�@e�T@e�@e/@d�@d�/@d�j@d��@dz�@dj@dj@dj@dZ@d(�@c�F@c"�@co@b�@b�@b�@b�@c@b�@bJ@ax�@a&�@`�`@`Q�@_�@^��@^�y@^ȴ@^��@^$�@]`B@]�@\�/@\Z@\(�@[��@[@Z��@Z-@Yx�@Y7L@Y�@Y%@X��@XĜ@Xr�@Xb@W�w@W|�@V�@VE�@U@U�-@U`B@T��@Tz�@T(�@S�m@S�@S@R-@R�@R�@Q��@P�@O�w@OK�@O�@Nv�@M��@M?}@L��@L(�@K�
@K�F@KC�@Jn�@J=q@J�@I��@I��@I�@HQ�@HQ�@HQ�@HA�@G�w@G
=@F��@Fv�@F5?@F{@E�T@E��@E�@D��@Dj@D9X@D�@C�m@C"�@B�H@B~�@A��@A��@A�@A�#@A�^@A�7@Ax�@AX@@��@@A�@?�@?�@>��@>��@>5?@=��@=p�@<�j@<�D@<1@;��@;S�@:��@:�\@:n�@:-@9�@9�#@9�#@9�^@9�7@97L@8Ĝ@8r�@7�;@7�@7��@7�P@6��@6��@6ff@6V@6V@65?@5��@5�@5O�@5V@4�j@4�D@4Z@49X@4�@3�
@3dZ@2�H@2�!@2�\@2M�@2J@1��@0��@0��@0��@0��@0�u@0Q�@0 �@/�@/+@.ff@-��@-�h@-�h@-�h@-�h@-p�@-`B@-?}@,�@,z�@,I�@,�@+�
@+��@+��@+dZ@*�!@*M�@)��@)X@)&�@)�@(�`@(��@(Q�@'�@'|�@'\)@'�@'�@'
=@'
=@&�y@&��@&��@&��@&�+@&v�@&ff@&5?@%@%�-@%��@%?}@$��@$��@$Z@#�F@#dZ@#o@"�H@"^5@"�@!�#@!�@ Ĝ@ �9@ ��@ �u@ �@ 1'@��@��@\)@\)@\)@\)@\)@K�@
=@v�@V@V@E�@E�@E�@5?@$�@@�T@p�@V@Z@�m@�
@�
@ƨ@��@S�@"�@o@�H@�!@��@M�@�#@�7@x�@G�@�`@��@�@Q�@b@��@�@l�@K�@
=@�@��@E�@@�h@�j@z�@j@Z@�@ƨ@�@t�@dZ@33@�H@~�@n�@^5@=q@�@��@�#@��@�7@G�@&�@��@�@r�@Q�@1'@b@b@b@�@�w@|�@;d@+@�@�@ȴ@�R@ȴ@ȴ@�R@V@�T@@��@��@��@��@�@�@V@V@V@��@�@��@�@j@9X@1@��@ƨ@�F@�@S�@C�@@
�!@
��@
��@
��@
��@
��@
��@
�\@
^5@
M�@
M�@
�@	�#@	��@	��@	��@	�^@	�7@	�7@	x�@	x�@	hs@	hs@	hs@	X@	&�@Ĝ@�@Q�@b@  @�@�@�;@�w@�P@\)@K�@K�@K�@K�@K�111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	(�B	(�B	(�B	(�B	(�B	)�B	)�B	+B	+B	,B	-B	.B	1'B	L�B	�qB	�B
#�B
.B
(�B
>wB
ffB
q�B
w�B
� B
�BB�B�B�B�B�B.B:^B7LB7LB-Bt�B~�Bq�B^5BN�B33B7LB0!B%�B,B&�B�BoBB��B�ZB�B�ZB��B�Bx�Bo�BXB+B
�B
ƨB
�?B
��B
�DB
iyB
E�B
:^B
�B
VB	�ZB	��B	ÖB	�oB	�JB	r�B	hsB	YB	H�B	/B��B�
B�
B�fB�B�yB�;B�#B�B�B�B�
B��B�qB�LB��B��B�dB�^B�3B��B�9B�FB�B�^B�jBÖB�B	DB	B	oB	DB	\B	#�B	0!B	E�B	cTB	m�B	q�B	}�B	�DB	�PB	�PB	�VB	�{B	�hB	�DB	}�B	�7B	�PB	�VB	�uB	�uB	�oB	�bB	�DB	�\B	�DB	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	�!B	�B	�BB	�;B	�5B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B
  B
B	��B	��B	��B
  B
B	��B
%B
1B
+B
DB
VB
\B
bB
JB
+B
JB
\B
{B
�B
�B
�B
�B
�B
�B
�B
�B
oB
\B
\B
�B
uB
uB
uB
oB
bB
VB
\B
bB
PB
	7B
B	��B
  B
  B	��B
  B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B	��B
  B
B
B
B
B
B	��B	��B
  B	��B	��B
  B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B	��B
B
B
%B
%B
+B
+B
	7B
	7B

=B
DB

=B

=B

=B

=B
1B
+B
1B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
JB
JB
DB

=B
	7B
DB
PB
VB
JB
PB
VB
\B
bB
hB
hB
{B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
 �B
�B
�B
 �B
!�B
!�B
$�B
%�B
%�B
"�B
!�B
#�B
$�B
$�B
$�B
#�B
�B
�B
%�B
$�B
#�B
#�B
'�B
'�B
'�B
(�B
)�B
(�B
&�B
&�B
)�B
+B
+B
)�B
(�B
'�B
(�B
'�B
'�B
+B
)�B
+B
+B
'�B
)�B
+B
)�B
+B
)�B
,B
.B
/B
.B
.B
.B
/B
/B
/B
0!B
2-B
1'B
0!B
1'B
1'B
2-B
49B
49B
49B
49B
5?B
49B
49B
5?B
5?B
49B
7LB
9XB
8RB
8RB
9XB
:^B
9XB
8RB
8RB
8RB
8RB
6FB
8RB
7LB
9XB
7LB
:^B
=qB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
?}B
@�B
A�B
B�B
B�B
B�B
B�B
A�B
A�B
B�B
D�B
D�B
C�B
B�B
@�B
?}B
C�B
A�B
B�B
E�B
E�B
E�B
D�B
D�B
C�B
B�B
A�B
C�B
D�B
F�B
F�B
F�B
E�B
I�B
I�B
I�B
I�B
H�B
H�B
F�B
G�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
L�B
K�B
I�B
I�B
K�B
O�B
Q�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
S�B
S�B
T�B
VB
W
B
W
B
VB
T�B
T�B
VB
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
XB
XB
XB
YB
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
ZB
[#B
\)B
\)B
[#B
ZB
^5B
_;B
_;B
^5B
]/B
]/B
_;B
_;B
_;B
_;B
_;B
^5B
`BB
_;B
_;B
aHB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
e`B
dZB
aHB
dZB
e`B
ffB
e`B
e`B
ffB
gmB
ffB
hsB
hsB
gmB
gmB
iyB
iyB
iyB
iyB
hsB
hsB
k�B
k�B
jB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
jB
k�B
l�B
m�B
m�B
l�B
k�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
m�B
m�B
o�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
t�B
w�B
x�B
w�B
w�B
v�B
v�B
v�B
v�B
v�B
v�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
z�B
y�B
z�B
{�B
|�B
}�B
~�B
~�B
}�B
|�B
|�B
~�B
~�B
~�B
� B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�%B
�+B
�1B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�JB
�PB
�PB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�\B
�\B
�\B
�bB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�hB
�bB
�bB
�oB
�oB
�uB
�uB
�oB
�oB
�oB
�uB
�{B
�{B
�uB
�uB
�uB
�uB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	'�B	(�B	(�B	(�B	(�B	(�B	)�B	)�B	+B	+B	,B	-B	.B	1'B	L�B	�qB	�B
#�B
.B
(�B
>wB
ffB
q�B
w�B
� B
�BB�B�B�B�B�B.B:^B7LB7LB-Bt�B~�Bq�B^5BN�B33B7LB0!B%�B,B&�B�BoBB��B�ZB�B�ZB��B�Bx�Bo�BXB+B
�B
ƨB
�?B
��B
�DB
iyB
E�B
:^B
�B
VB	�ZB	��B	ÖB	�oB	�JB	r�B	hsB	YB	H�B	/B��B�
B�
B�fB�B�yB�;B�#B�B�B�B�
B��B�qB�LB��B��B�dB�^B�3B��B�9B�FB�B�^B�jBÖB�B	DB	B	oB	DB	\B	#�B	0!B	E�B	cTB	m�B	q�B	}�B	�DB	�PB	�PB	�VB	�{B	�hB	�DB	}�B	�7B	�PB	�VB	�uB	�uB	�oB	�bB	�DB	�\B	�DB	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	�!B	�B	�BB	�;B	�5B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B
  B
B	��B	��B	��B
  B
B	��B
%B
1B
+B
DB
VB
\B
bB
JB
+B
JB
\B
{B
�B
�B
�B
�B
�B
�B
�B
�B
oB
\B
\B
�B
uB
uB
uB
oB
bB
VB
\B
bB
PB
	7B
B	��B
  B
  B	��B
  B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B	��B
  B
B
B
B
B
B	��B	��B
  B	��B	��B
  B	��B	��B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B	��B
B
B
%B
%B
+B
+B
	7B
	7B

=B
DB

=B

=B

=B

=B
1B
+B
1B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
JB
JB
DB

=B
	7B
DB
PB
VB
JB
PB
VB
\B
bB
hB
hB
{B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
 �B
�B
�B
 �B
!�B
!�B
$�B
%�B
%�B
"�B
!�B
#�B
$�B
$�B
$�B
#�B
�B
�B
%�B
$�B
#�B
#�B
'�B
'�B
'�B
(�B
)�B
(�B
&�B
&�B
)�B
+B
+B
)�B
(�B
'�B
(�B
'�B
'�B
+B
)�B
+B
+B
'�B
)�B
+B
)�B
+B
)�B
,B
.B
/B
.B
.B
.B
/B
/B
/B
0!B
2-B
1'B
0!B
1'B
1'B
2-B
49B
49B
49B
49B
5?B
49B
49B
5?B
5?B
49B
7LB
9XB
8RB
8RB
9XB
:^B
9XB
8RB
8RB
8RB
8RB
6FB
8RB
7LB
9XB
7LB
:^B
=qB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
?}B
@�B
A�B
B�B
B�B
B�B
B�B
A�B
A�B
B�B
D�B
D�B
C�B
B�B
@�B
?}B
C�B
A�B
B�B
E�B
E�B
E�B
D�B
D�B
C�B
B�B
A�B
C�B
D�B
F�B
F�B
F�B
E�B
I�B
I�B
I�B
I�B
H�B
H�B
F�B
G�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
L�B
K�B
I�B
I�B
K�B
O�B
Q�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
S�B
S�B
T�B
VB
W
B
W
B
VB
T�B
T�B
VB
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
XB
XB
XB
YB
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
ZB
[#B
\)B
\)B
[#B
ZB
^5B
_;B
_;B
^5B
]/B
]/B
_;B
_;B
_;B
_;B
_;B
^5B
`BB
_;B
_;B
aHB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
e`B
dZB
aHB
dZB
e`B
ffB
e`B
e`B
ffB
gmB
ffB
hsB
hsB
gmB
gmB
iyB
iyB
iyB
iyB
hsB
hsB
k�B
k�B
jB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
jB
k�B
l�B
m�B
m�B
l�B
k�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
m�B
m�B
o�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
t�B
w�B
x�B
w�B
w�B
v�B
v�B
v�B
v�B
v�B
v�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
z�B
y�B
z�B
{�B
|�B
}�B
~�B
~�B
}�B
|�B
|�B
~�B
~�B
~�B
� B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�%B
�+B
�1B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�JB
�PB
�PB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�\B
�\B
�\B
�bB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�hB
�bB
�bB
�oB
�oB
�uB
�uB
�oB
�oB
�oB
�uB
�{B
�{B
�uB
�uB
�uB
�uB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221212100111                              AO  ARCAADJP                                                                    20221212100111    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221212100111  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221212100111  QCF$                G�O�G�O�G�O�4000            