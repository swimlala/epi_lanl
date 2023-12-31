CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-25T09:01:27Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220725090127  20220725090127  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @������1   @��ӟI�V@'�~��"��daG�z�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @xQ�@�\)@�\)A�A?�A_�A�A��
A��
A��
A�
=A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�BhQ�Bo�Bw�B�B���B���B���B���B���B�B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D߂�D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�B�D炏D�\D��\D�?\D肏D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��)D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aۙ�Aۛ�Aۙ�Aۙ�Aۙ�A۝�A۟�A۝�AۓuAۑhAۍPAۉ7Aۇ+A�`BA�VA�dZA�C�A�
=A��/AڼjAڬAڝ�Aڇ+A�I�A��A��/A�ȴA�ƨAپwAټjA���A�Aٺ^Aٴ9Aٰ!A٬A�Q�A��AƼjA�oA��wA�^5A���A�;dA��^A���A�l�A�VA�(�A�JA��HA��DA�ƨA�O�A��A�9XA�I�A�A�A�M�A��PA�M�A�|�A�  A���A��A��!A�dZA�Q�A���A~ZAv��Ao�Af9XAbĜA\�AT(�AM��ALJAH�HAC\)AA\)AAhsAAƨAB �A?�A=?}A:�jA7��A6v�A6E�A5`BA4�A3+A1�A0�A0�A/t�A.�RA.I�A-�A,ȴA,9XA+��A+�A*�A)�A)�A)�A(�A'\)A'
=A&�uA&ZA&VA%��A%��A%�A$bNA#��A#|�A"��A"�9A"z�A"�A!l�A ��A �A�;A�7A�A��A9XAA�A$�A��Ax�A�7A��At�A��A9XAp�A�HAjA5?Ax�A��AE�AJA�hA�A��AI�A��A��A��AoA�uAffA�A��At�AhsAS�A?}A33A�A��A�uAffA-AA��A��A|�Al�AS�A33A�yA��Ar�A�A�^AS�AoAȴA��AM�A�A��AhsAC�A�A
��A
A	�TA	��A	�7A	"�A��A��A�A��A��AVA��A�9A�\AQ�A1A�^A\)A�`A�A5?A�PA?}A%A�+AffAbNAI�AA�A=qA1'A{A1AA�A�A/A j@��F@�5?@���@���@�r�@��@���@��@�"�@�n�@��@�Ĝ@�A�@���@�=q@��@��@���@�r�@�9X@� �@��@�ȴ@�x�@���@��D@� �@�@�S�@�K�@�=q@�x�@���@���@���@�-@�&�@���@�bN@�;d@�R@�+@�M�@�-@�{@�J@��@��T@���@���@�-@�X@���@�K�@�/@�z�@���@�33@��@���@�ff@���@ݩ�@���@��@���@�v�@��@�X@ش9@���@���@�/@Ԭ@Ӿw@ҟ�@�=q@ёh@��`@�r�@��m@Ώ\@�$�@�J@���@�1'@�+@��H@ʟ�@�^5@��#@ɑh@�hs@�Ĝ@Ǿw@�o@Ə\@�J@�x�@���@�A�@�ƨ@�dZ@�33@��@�v�@�/@�%@���@��j@�Q�@��@��
@���@�"�@�ȴ@�n�@�^5@�M�@�E�@�$�@�@�p�@�V@��/@��j@��9@��9@���@��@���@��
@���@�@�x�@���@�j@��;@���@�\)@�ȴ@���@�G�@��D@�A�@�1@��;@��@�~�@�=q@�$�@�@�@���@��h@�/@��/@�r�@�bN@�bN@�I�@��@�~�@��T@���@�`B@�G�@�%@��@��@��P@�t�@�"�@�~�@���@�j@�ƨ@���@�dZ@�;d@��@���@�n�@��-@�O�@��9@�A�@��@��@�K�@�
=@��@�v�@�v�@�n�@���@��h@��h@�hs@���@�z�@��m@�t�@��@�~�@�5?@�{@��@�@��@�Ĝ@��
@��F@��@�C�@�33@�o@��y@���@�J@��T@��^@�X@�G�@��@��@�Ĝ@��D@�b@��F@��P@�l�@�+@��H@���@��@�@��^@���@�7L@���@�z�@�9X@�  @�ƨ@���@�l�@�;d@��y@��+@�$�@���@��-@��7@�O�@�%@���@�1@�ƨ@�|�@�K�@��@��R@���@�=q@��@��h@�G�@�7L@�/@��@��u@�b@��F@�l�@�;d@��H@�~�@�ff@�V@�M�@�$�@��@��^@��@�X@��@���@��u@��u@�A�@�1@�  @�  @��@��@�S�@�C�@�C�@�C�@�"�@�ȴ@�V@���@���@�p�@��@��`@��@�z�@�I�@��m@��F@��@���@��@�C�@��@���@�~�@�=q@��@��@��h@�hs@�V@��u@�9X@�@�w@�P@K�@~�y@~E�@}��@}V@|�@|��@|z�@|(�@|1@{��@{�
@{C�@{@z�@z��@y��@x�u@x �@x  @x  @w�w@w;d@v��@v$�@u��@uO�@u?}@uV@t��@tz�@tZ@t9X@s�m@s�F@s��@s33@s@r��@r�!@r~�@r^5@q��@p�`@p��@pr�@pb@o�@o�P@o+@n��@m�@m�@m?}@l�@l��@l�j@l��@l1@k�@k"�@j��@j~�@jJ@i�7@i&�@h��@hĜ@h��@hr�@hbN@hA�@g�;@g�@gl�@g;d@fȴ@fV@e�@ep�@d��@d�@d�j@d�@c��@b��@bJ@a%@`b@_�;@_\)@^��@^��@^$�@]�@]O�@]V@\�/@\�@\9X@[��@[C�@[@Z��@Z��@Y�#@Y�@X��@X�9@X�u@X�@Xr�@XbN@XA�@W�;@Wl�@W+@W+@W+@V��@V��@Vv�@Vff@Vv�@Vff@Vff@Vff@VV@V$�@U�h@T��@T�@Sƨ@S��@St�@S33@S"�@S"�@S"�@So@R�!@R-@Q��@Q�^@Q&�@P�9@Pr�@O��@O
=@N�+@M�T@Mp�@Lj@K�m@K�
@K��@J�H@I�#@I��@I7L@I&�@H�`@H�u@Hr�@HQ�@H �@G\)@G�@Fȴ@F5?@E�@E�@EV@D�@Dz�@C��@C��@CC�@B�H@B��@B^5@A�@A��@A�7@A7L@@��@@��@@bN@?�@?�P@?\)@?K�@?�@>�@>v�@>$�@=��@=�@=`B@=?}@=�@<��@<�j@<��@<(�@;��@;dZ@;33@;33@;o@;@:�!@:J@9X@97L@8�`@8A�@7�@7��@7�w@7K�@6�@6�R@6��@6ff@6V@6{@5��@5�-@5�-@5�-@5�h@5�@4��@4��@4��@4��@4��@4��@4I�@4(�@4�@3�m@3��@3t�@3S�@3"�@2�H@2��@2^5@1��@1��@1��@1x�@1X@17L@1&�@1%@0��@0�@/�;@/\)@/+@.�@.��@.�+@.$�@-��@-��@-�-@-p�@-V@,��@,�/@,j@,I�@+�
@+t�@+@*~�@*=q@)��@)7L@(��@(�u@'�;@&��@&E�@&@%��@%`B@%V@$�j@$z�@$Z@$�@#��@"�@"��@"�!@"~�@"=q@"J@!�@!�@!�#@!��@!x�@!&�@ ��@ r�@  �@�@�w@��@l�@;d@�@ff@{@@�@@�@��@�@?}@/@/@�@�j@�j@�@z�@9X@�@�
@dZ@C�@33@o@o@�@�H@�H@��@��@��@��@�7@x�@G�@&�@��@�9@1'@  @�@��@��@�P@|�@|�@|�@l�@K�@;d@��@�R@�+@�T@`B@O�@`B@`B@`B@`B@`B@?}@�@�D@Z@9X@��@�F@��@��@t�@C�@�@��@��@~�@^5@M�@�@�7@hs@X@7L@��@�9@��@�u@bN@1'@�@�@�P@l�@;d@�y@�R@v�@V@E�@5?@@�T@�T@�T@@�-@�@p�@?}@�@V@�@��@�D@z�@1@��@dZ@C�@o@
��@
��1111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aۙ�Aۛ�Aۙ�Aۙ�Aۙ�A۝�A۟�A۝�AۓuAۑhAۍPAۉ7Aۇ+A�`BA�VA�dZA�C�A�
=A��/AڼjAڬAڝ�Aڇ+A�I�A��A��/A�ȴA�ƨAپwAټjA���A�Aٺ^Aٴ9Aٰ!A٬A�Q�A��AƼjA�oA��wA�^5A���A�;dA��^A���A�l�A�VA�(�A�JA��HA��DA�ƨA�O�A��A�9XA�I�A�A�A�M�A��PA�M�A�|�A�  A���A��A��!A�dZA�Q�A���A~ZAv��Ao�Af9XAbĜA\�AT(�AM��ALJAH�HAC\)AA\)AAhsAAƨAB �A?�A=?}A:�jA7��A6v�A6E�A5`BA4�A3+A1�A0�A0�A/t�A.�RA.I�A-�A,ȴA,9XA+��A+�A*�A)�A)�A)�A(�A'\)A'
=A&�uA&ZA&VA%��A%��A%�A$bNA#��A#|�A"��A"�9A"z�A"�A!l�A ��A �A�;A�7A�A��A9XAA�A$�A��Ax�A�7A��At�A��A9XAp�A�HAjA5?Ax�A��AE�AJA�hA�A��AI�A��A��A��AoA�uAffA�A��At�AhsAS�A?}A33A�A��A�uAffA-AA��A��A|�Al�AS�A33A�yA��Ar�A�A�^AS�AoAȴA��AM�A�A��AhsAC�A�A
��A
A	�TA	��A	�7A	"�A��A��A�A��A��AVA��A�9A�\AQ�A1A�^A\)A�`A�A5?A�PA?}A%A�+AffAbNAI�AA�A=qA1'A{A1AA�A�A/A j@��F@�5?@���@���@�r�@��@���@��@�"�@�n�@��@�Ĝ@�A�@���@�=q@��@��@���@�r�@�9X@� �@��@�ȴ@�x�@���@��D@� �@�@�S�@�K�@�=q@�x�@���@���@���@�-@�&�@���@�bN@�;d@�R@�+@�M�@�-@�{@�J@��@��T@���@���@�-@�X@���@�K�@�/@�z�@���@�33@��@���@�ff@���@ݩ�@���@��@���@�v�@��@�X@ش9@���@���@�/@Ԭ@Ӿw@ҟ�@�=q@ёh@��`@�r�@��m@Ώ\@�$�@�J@���@�1'@�+@��H@ʟ�@�^5@��#@ɑh@�hs@�Ĝ@Ǿw@�o@Ə\@�J@�x�@���@�A�@�ƨ@�dZ@�33@��@�v�@�/@�%@���@��j@�Q�@��@��
@���@�"�@�ȴ@�n�@�^5@�M�@�E�@�$�@�@�p�@�V@��/@��j@��9@��9@���@��@���@��
@���@�@�x�@���@�j@��;@���@�\)@�ȴ@���@�G�@��D@�A�@�1@��;@��@�~�@�=q@�$�@�@�@���@��h@�/@��/@�r�@�bN@�bN@�I�@��@�~�@��T@���@�`B@�G�@�%@��@��@��P@�t�@�"�@�~�@���@�j@�ƨ@���@�dZ@�;d@��@���@�n�@��-@�O�@��9@�A�@��@��@�K�@�
=@��@�v�@�v�@�n�@���@��h@��h@�hs@���@�z�@��m@�t�@��@�~�@�5?@�{@��@�@��@�Ĝ@��
@��F@��@�C�@�33@�o@��y@���@�J@��T@��^@�X@�G�@��@��@�Ĝ@��D@�b@��F@��P@�l�@�+@��H@���@��@�@��^@���@�7L@���@�z�@�9X@�  @�ƨ@���@�l�@�;d@��y@��+@�$�@���@��-@��7@�O�@�%@���@�1@�ƨ@�|�@�K�@��@��R@���@�=q@��@��h@�G�@�7L@�/@��@��u@�b@��F@�l�@�;d@��H@�~�@�ff@�V@�M�@�$�@��@��^@��@�X@��@���@��u@��u@�A�@�1@�  @�  @��@��@�S�@�C�@�C�@�C�@�"�@�ȴ@�V@���@���@�p�@��@��`@��@�z�@�I�@��m@��F@��@���@��@�C�@��@���@�~�@�=q@��@��@��h@�hs@�V@��u@�9X@�@�w@�P@K�@~�y@~E�@}��@}V@|�@|��@|z�@|(�@|1@{��@{�
@{C�@{@z�@z��@y��@x�u@x �@x  @x  @w�w@w;d@v��@v$�@u��@uO�@u?}@uV@t��@tz�@tZ@t9X@s�m@s�F@s��@s33@s@r��@r�!@r~�@r^5@q��@p�`@p��@pr�@pb@o�@o�P@o+@n��@m�@m�@m?}@l�@l��@l�j@l��@l1@k�@k"�@j��@j~�@jJ@i�7@i&�@h��@hĜ@h��@hr�@hbN@hA�@g�;@g�@gl�@g;d@fȴ@fV@e�@ep�@d��@d�@d�j@d�@c��@b��@bJ@a%@`b@_�;@_\)@^��@^��@^$�@]�@]O�@]V@\�/@\�@\9X@[��@[C�@[@Z��@Z��@Y�#@Y�@X��@X�9@X�u@X�@Xr�@XbN@XA�@W�;@Wl�@W+@W+@W+@V��@V��@Vv�@Vff@Vv�@Vff@Vff@Vff@VV@V$�@U�h@T��@T�@Sƨ@S��@St�@S33@S"�@S"�@S"�@So@R�!@R-@Q��@Q�^@Q&�@P�9@Pr�@O��@O
=@N�+@M�T@Mp�@Lj@K�m@K�
@K��@J�H@I�#@I��@I7L@I&�@H�`@H�u@Hr�@HQ�@H �@G\)@G�@Fȴ@F5?@E�@E�@EV@D�@Dz�@C��@C��@CC�@B�H@B��@B^5@A�@A��@A�7@A7L@@��@@��@@bN@?�@?�P@?\)@?K�@?�@>�@>v�@>$�@=��@=�@=`B@=?}@=�@<��@<�j@<��@<(�@;��@;dZ@;33@;33@;o@;@:�!@:J@9X@97L@8�`@8A�@7�@7��@7�w@7K�@6�@6�R@6��@6ff@6V@6{@5��@5�-@5�-@5�-@5�h@5�@4��@4��@4��@4��@4��@4��@4I�@4(�@4�@3�m@3��@3t�@3S�@3"�@2�H@2��@2^5@1��@1��@1��@1x�@1X@17L@1&�@1%@0��@0�@/�;@/\)@/+@.�@.��@.�+@.$�@-��@-��@-�-@-p�@-V@,��@,�/@,j@,I�@+�
@+t�@+@*~�@*=q@)��@)7L@(��@(�u@'�;@&��@&E�@&@%��@%`B@%V@$�j@$z�@$Z@$�@#��@"�@"��@"�!@"~�@"=q@"J@!�@!�@!�#@!��@!x�@!&�@ ��@ r�@  �@�@�w@��@l�@;d@�@ff@{@@�@@�@��@�@?}@/@/@�@�j@�j@�@z�@9X@�@�
@dZ@C�@33@o@o@�@�H@�H@��@��@��@��@�7@x�@G�@&�@��@�9@1'@  @�@��@��@�P@|�@|�@|�@l�@K�@;d@��@�R@�+@�T@`B@O�@`B@`B@`B@`B@`B@?}@�@�D@Z@9X@��@�F@��@��@t�@C�@�@��@��@~�@^5@M�@�@�7@hs@X@7L@��@�9@��@�u@bN@1'@�@�@�P@l�@;d@�y@�R@v�@V@E�@5?@@�T@�T@�T@@�-@�@p�@?}@�@V@�@��@�D@z�@1@��@dZ@C�@o@
��@
��1111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
E�B
Q�B
R�B
N�B
P�B
[#B
bNB
jB
l�B
m�B
o�B
y�B
�B
�hB
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
x�B
�qBP�BC�BJ�B^5BaHBaHB��B|�B|�B�=B�Bo�BA�BE�B�Bu�B=qB5?B@�B:^B/B"�B
��B
�DB
I�B
0!B
(�B
'�B	�5B	�wB	��B	��B	{�B	@�B	L�B	DB�B�B	{B��B�B		7B	<jB	^5B	l�B	^5B	cTB	^5B	R�B	dZB	u�B	p�B	x�B	m�B	o�B	w�B	� B	�B	�%B	�VB	��B	��B	��B	�B	�FB	�'B	�-B	��B	�}B	�dB	ȴB	��B	��B	�/B	�TB	�NB	�HB	�NB	�fB	�B	��B	��B	��B
B
1B
	7B
hB
�B
 �B
#�B
#�B
&�B
+B
.B
0!B
5?B
;dB
C�B
F�B
G�B
D�B
>wB
B�B
?}B
A�B
A�B
C�B
=qB
?}B
D�B
H�B
D�B
A�B
H�B
H�B
H�B
J�B
I�B
F�B
F�B
M�B
K�B
I�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
M�B
M�B
O�B
M�B
O�B
N�B
O�B
Q�B
R�B
P�B
O�B
L�B
L�B
M�B
J�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
O�B
R�B
R�B
R�B
P�B
I�B
S�B
S�B
O�B
L�B
M�B
M�B
I�B
L�B
M�B
I�B
N�B
Q�B
O�B
N�B
K�B
K�B
J�B
G�B
I�B
G�B
C�B
E�B
I�B
E�B
K�B
M�B
L�B
L�B
L�B
K�B
J�B
J�B
I�B
F�B
A�B
<jB
5?B
9XB
5?B
=qB
;dB
?}B
?}B
@�B
=qB
9XB
7LB
1'B
<jB
9XB
1'B
;dB
=qB
<jB
:^B
<jB
<jB
;dB
7LB
2-B
0!B
33B
7LB
5?B
49B
6FB
5?B
/B
-B
0!B
+B
(�B
+B
'�B
/B
-B
)�B
-B
1'B
2-B
2-B
33B
33B
33B
2-B
2-B
1'B
/B
+B
&�B
�B
�B
%�B
)�B
'�B
+B
,B
)�B
(�B
'�B
$�B
!�B
�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
{B
�B
!�B
!�B
!�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
 �B
�B
�B
�B
 �B
!�B
!�B
!�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
�B
"�B
$�B
$�B
#�B
$�B
#�B
!�B
!�B
"�B
$�B
%�B
"�B
�B
�B
�B
#�B
"�B
#�B
"�B
 �B
"�B
!�B
%�B
"�B
 �B
�B
�B
"�B
'�B
'�B
'�B
%�B
%�B
$�B
"�B
%�B
#�B
%�B
&�B
&�B
(�B
(�B
(�B
(�B
,B
+B
(�B
)�B
,B
+B
'�B
&�B
&�B
'�B
(�B
+B
-B
.B
.B
-B
)�B
+B
)�B
/B
/B
/B
0!B
0!B
/B
-B
,B
/B
0!B
/B
1'B
1'B
1'B
1'B
0!B
/B
0!B
2-B
2-B
2-B
1'B
1'B
0!B
2-B
5?B
33B
1'B
0!B
33B
33B
5?B
5?B
5?B
5?B
49B
33B
33B
5?B
5?B
7LB
7LB
6FB
6FB
5?B
49B
8RB
9XB
9XB
9XB
;dB
;dB
9XB
:^B
;dB
<jB
>wB
>wB
<jB
:^B
9XB
;dB
<jB
>wB
>wB
>wB
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
A�B
C�B
@�B
A�B
D�B
C�B
A�B
D�B
E�B
F�B
F�B
E�B
C�B
A�B
A�B
A�B
D�B
E�B
D�B
F�B
F�B
F�B
G�B
E�B
H�B
I�B
I�B
H�B
F�B
F�B
H�B
I�B
I�B
J�B
J�B
I�B
I�B
I�B
G�B
J�B
K�B
M�B
M�B
L�B
K�B
K�B
L�B
L�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
M�B
O�B
O�B
N�B
J�B
J�B
N�B
P�B
P�B
O�B
N�B
N�B
O�B
P�B
P�B
R�B
R�B
Q�B
S�B
S�B
R�B
R�B
R�B
S�B
R�B
S�B
S�B
T�B
S�B
R�B
Q�B
Q�B
T�B
T�B
S�B
S�B
T�B
S�B
S�B
S�B
VB
W
B
W
B
XB
XB
XB
VB
W
B
XB
XB
YB
XB
XB
ZB
[#B
[#B
\)B
[#B
\)B
[#B
ZB
[#B
ZB
ZB
YB
YB
ZB
ZB
[#B
\)B
[#B
ZB
YB
YB
YB
ZB
ZB
^5B
]/B
^5B
^5B
^5B
_;B
^5B
`BB
`BB
`BB
_;B
_;B
aHB
aHB
aHB
aHB
_;B
`BB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
bNB
bNB
dZB
e`B
e`B
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
dZB
cTB
bNB
aHB
cTB
e`B
gmB
gmB
gmB
hsB
hsB
gmB
gmB
e`B
e`B
gmB
ffB
e`B
ffB
ffB
dZB
ffB
ffB
ffB
gmB
e`B
hsB
jB
iyB
gmB
gmB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
jB
l�B
l�B
k�B
l�B
n�B
o�B
o�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
o�B
q�B
q�B
p�B
p�B
p�B
q�B
p�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
u�B
u�B
u�B
u�B
t�B
t�B
s�B
s�B
u�B
u�B
v�B
u�B
u�B
s�B
r�B
r�B
u�B
u�B
s�B
v�B
w�B
w�B
v�B
v�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
z�B
y�B
y�B
x�B
y�B
z�B
{�B
z�B
z�B
y�B
x�B
z�B
z�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
z�B
z�B
{�B
}�B
}�B
}�B
~�B
}�B
~�B
� B
� B
~�B
~�B
� B
� B
~�B
~�B
~�B
~�B
~�B
~�B
� B
~�B
~�B
� B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�%B
�+B
�%B
�%B
�B
�B
�%B
�B
�%B
�%B
�%B
�+B
�%B
�%B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�+B
�+B
�1B
�7B
�1B
�1B
�7B
�=B
�7B
�7B
�7B
�7B
�1B
�=B
�DB
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�7B
�1B
�7B
�DB
�DB
�DB
�=B
�DB
�=B
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�DB
�DB
�DB
�=B
�DB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�JB
�JB
�VB
�VB
�VB
�VB
�\B
�\B
�VB
�VB
�VB
�\B
�bB
�\B
�\B
�\B
�\B
�\B
�bB
�hB
�bB
�bB
�bB
�oB
�oB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
��B
��B
�{B
�{B
�{B
��B
�{B
��B
��B
�{B
��B
��B
��B
�{B
�{B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
E�B
Q�B
R�B
N�B
P�B
[#B
bNB
jB
l�B
m�B
o�B
y�B
�B
�hB
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
x�B
�qBP�BC�BJ�B^5BaHBaHB��B|�B|�B�=B�Bo�BA�BE�B�Bu�B=qB5?B@�B:^B/B"�B
��B
�DB
I�B
0!B
(�B
'�B	�5B	�wB	��B	��B	{�B	@�B	L�B	DB�B�B	{B��B�B		7B	<jB	^5B	l�B	^5B	cTB	^5B	R�B	dZB	u�B	p�B	x�B	m�B	o�B	w�B	� B	�B	�%B	�VB	��B	��B	��B	�B	�FB	�'B	�-B	��B	�}B	�dB	ȴB	��B	��B	�/B	�TB	�NB	�HB	�NB	�fB	�B	��B	��B	��B
B
1B
	7B
hB
�B
 �B
#�B
#�B
&�B
+B
.B
0!B
5?B
;dB
C�B
F�B
G�B
D�B
>wB
B�B
?}B
A�B
A�B
C�B
=qB
?}B
D�B
H�B
D�B
A�B
H�B
H�B
H�B
J�B
I�B
F�B
F�B
M�B
K�B
I�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
M�B
M�B
O�B
M�B
O�B
N�B
O�B
Q�B
R�B
P�B
O�B
L�B
L�B
M�B
J�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
O�B
R�B
R�B
R�B
P�B
I�B
S�B
S�B
O�B
L�B
M�B
M�B
I�B
L�B
M�B
I�B
N�B
Q�B
O�B
N�B
K�B
K�B
J�B
G�B
I�B
G�B
C�B
E�B
I�B
E�B
K�B
M�B
L�B
L�B
L�B
K�B
J�B
J�B
I�B
F�B
A�B
<jB
5?B
9XB
5?B
=qB
;dB
?}B
?}B
@�B
=qB
9XB
7LB
1'B
<jB
9XB
1'B
;dB
=qB
<jB
:^B
<jB
<jB
;dB
7LB
2-B
0!B
33B
7LB
5?B
49B
6FB
5?B
/B
-B
0!B
+B
(�B
+B
'�B
/B
-B
)�B
-B
1'B
2-B
2-B
33B
33B
33B
2-B
2-B
1'B
/B
+B
&�B
�B
�B
%�B
)�B
'�B
+B
,B
)�B
(�B
'�B
$�B
!�B
�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
{B
�B
!�B
!�B
!�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
 �B
�B
�B
�B
 �B
!�B
!�B
!�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
�B
"�B
$�B
$�B
#�B
$�B
#�B
!�B
!�B
"�B
$�B
%�B
"�B
�B
�B
�B
#�B
"�B
#�B
"�B
 �B
"�B
!�B
%�B
"�B
 �B
�B
�B
"�B
'�B
'�B
'�B
%�B
%�B
$�B
"�B
%�B
#�B
%�B
&�B
&�B
(�B
(�B
(�B
(�B
,B
+B
(�B
)�B
,B
+B
'�B
&�B
&�B
'�B
(�B
+B
-B
.B
.B
-B
)�B
+B
)�B
/B
/B
/B
0!B
0!B
/B
-B
,B
/B
0!B
/B
1'B
1'B
1'B
1'B
0!B
/B
0!B
2-B
2-B
2-B
1'B
1'B
0!B
2-B
5?B
33B
1'B
0!B
33B
33B
5?B
5?B
5?B
5?B
49B
33B
33B
5?B
5?B
7LB
7LB
6FB
6FB
5?B
49B
8RB
9XB
9XB
9XB
;dB
;dB
9XB
:^B
;dB
<jB
>wB
>wB
<jB
:^B
9XB
;dB
<jB
>wB
>wB
>wB
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
A�B
C�B
@�B
A�B
D�B
C�B
A�B
D�B
E�B
F�B
F�B
E�B
C�B
A�B
A�B
A�B
D�B
E�B
D�B
F�B
F�B
F�B
G�B
E�B
H�B
I�B
I�B
H�B
F�B
F�B
H�B
I�B
I�B
J�B
J�B
I�B
I�B
I�B
G�B
J�B
K�B
M�B
M�B
L�B
K�B
K�B
L�B
L�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
M�B
O�B
O�B
N�B
J�B
J�B
N�B
P�B
P�B
O�B
N�B
N�B
O�B
P�B
P�B
R�B
R�B
Q�B
S�B
S�B
R�B
R�B
R�B
S�B
R�B
S�B
S�B
T�B
S�B
R�B
Q�B
Q�B
T�B
T�B
S�B
S�B
T�B
S�B
S�B
S�B
VB
W
B
W
B
XB
XB
XB
VB
W
B
XB
XB
YB
XB
XB
ZB
[#B
[#B
\)B
[#B
\)B
[#B
ZB
[#B
ZB
ZB
YB
YB
ZB
ZB
[#B
\)B
[#B
ZB
YB
YB
YB
ZB
ZB
^5B
]/B
^5B
^5B
^5B
_;B
^5B
`BB
`BB
`BB
_;B
_;B
aHB
aHB
aHB
aHB
_;B
`BB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
bNB
bNB
dZB
e`B
e`B
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
dZB
cTB
bNB
aHB
cTB
e`B
gmB
gmB
gmB
hsB
hsB
gmB
gmB
e`B
e`B
gmB
ffB
e`B
ffB
ffB
dZB
ffB
ffB
ffB
gmB
e`B
hsB
jB
iyB
gmB
gmB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
jB
l�B
l�B
k�B
l�B
n�B
o�B
o�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
o�B
q�B
q�B
p�B
p�B
p�B
q�B
p�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
u�B
u�B
u�B
u�B
t�B
t�B
s�B
s�B
u�B
u�B
v�B
u�B
u�B
s�B
r�B
r�B
u�B
u�B
s�B
v�B
w�B
w�B
v�B
v�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
z�B
y�B
y�B
x�B
y�B
z�B
{�B
z�B
z�B
y�B
x�B
z�B
z�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
z�B
z�B
{�B
}�B
}�B
}�B
~�B
}�B
~�B
� B
� B
~�B
~�B
� B
� B
~�B
~�B
~�B
~�B
~�B
~�B
� B
~�B
~�B
� B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�%B
�+B
�%B
�%B
�B
�B
�%B
�B
�%B
�%B
�%B
�+B
�%B
�%B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�+B
�+B
�1B
�7B
�1B
�1B
�7B
�=B
�7B
�7B
�7B
�7B
�1B
�=B
�DB
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�7B
�1B
�7B
�DB
�DB
�DB
�=B
�DB
�=B
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�DB
�DB
�DB
�=B
�DB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�JB
�JB
�VB
�VB
�VB
�VB
�\B
�\B
�VB
�VB
�VB
�\B
�bB
�\B
�\B
�\B
�\B
�\B
�bB
�hB
�bB
�bB
�bB
�oB
�oB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
��B
��B
�{B
�{B
�{B
��B
�{B
��B
��B
�{B
��B
��B
��B
�{B
�{B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220725090127                              AO  ARCAADJP                                                                    20220725090127    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220725090127  QCP$                G�O�G�O�G�O�5F83E           AO  ARGQQCPL                                                                    20220725090127  QCF$                G�O�G�O�G�O�4000            