CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-09-03T09:01:21Z creation      
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
resolution        =���   axis      Z        T  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ڤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ݤ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220903090121  20220903090121  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��ա�W1   @���>��F@*�Z�1�de���o1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BXffB`  Bh��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ DӼ�D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�FfD�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BHQ�BO�BXQ�B_�Bh�RBo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\DӼ)D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�|)Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��D�E�D�b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A߇+AߍPAߓuAߕ�Aߕ�Aߙ�Aߕ�Aߣ�A߬A߬Aߩ�A߬A߶FA߶FAߴ9A߮Aߣ�A�z�A�E�A�A�A�=qA�5?A��A�A��;A�K�A��`Aڲ-A�M�AӰ!A�~�A�Q�A���A�|�A�VA��-A��A�p�A��A�%A���A�v�A�9XA�z�A��A��9A���A��!A��FA�$�A�bNA��A�Q�A���A�(�A��7A�ZA��+A�jA��wA��PA��A�  A���A���A�JA��A�dZA� �A�ZA�
=A���A~��A{VAv5?AtJAsS�Aq�Alv�Ag7LAb(�A^�uA\�AW�ATI�AR�/AP5?AM��AJ�AG��AA��A?x�A=hsA<A�A;��A:�RA9�wA9VA8VA8JA7�
A7�wA7��A7|�A7�A5+A3|�A2�!A0A�A//A.9XA-"�A+�A*��A*�/A*ȴA*M�A)ƨA)"�A)%A(��A(�uA'�;A'dZA&�\A&$�A%��A$�A#&�A"�+A"{A!�#A!��A!ƨA!l�A ��A�#A��A�wA�A�jA1A �AƨAt�AA$�A`BAĜAZAO�A��Av�AZA(�AA��A�-AhsA�A��A9XA��A�mA��A�A�A�!A��A�DAz�A$�A��A�AA�+AjA�Ap�AG�A7LA&�A�HA�jAv�A=qA5?A{A�A�-AS�A;dA�A�RA{A�A�wA�AO�A��A�!AI�A1A�wA;dA
�A
9XA	�7A	\)A	O�A	33A	
=A	A�A�/A��A�jA�DAr�A=qA�A��A�FAp�A%A�An�A=qA�
AdZA��AĜA��A$�AAO�A��AȴAI�A�mA  A�^A"�A ĜA �!A ��A Q�@��F@�\)@���@��7@�/@�%@��j@��D@�Q�@�b@�l�@�n�@�X@��9@�ƨ@��H@��@�?}@��@�D@�@���@��@�;d@���@�E�@���@�@��@�bN@�w@�C�@�~�@��@�@���@�?}@�j@��@�9@��@ꗍ@���@�@� �@�(�@� �@�b@�;d@�5?@�hs@�G�@���@�@��@�\@�=q@��@���@�G�@�@ߥ�@�o@ާ�@ݩ�@ܴ9@�Q�@� �@�b@�1@�1@���@ۥ�@�S�@ڧ�@��T@�O�@���@�9X@�o@֧�@֧�@�@�Ĝ@�1'@�b@��;@ӶF@�dZ@��@ҧ�@�ff@�V@ϥ�@��@�ff@�J@���@�`B@�?}@��@�I�@��m@˝�@�\)@��y@�G�@��/@���@���@ȼj@ȼj@ȣ�@�z�@��
@�K�@Ƈ+@�{@�&�@�\)@�v�@��#@�O�@���@���@�9X@��;@��P@�
=@��R@��\@��@��-@���@�p�@�&�@��/@�j@��m@�|�@�+@��@�M�@��T@��7@�?}@��u@��@��w@��P@���@��\@�V@�{@�@�@�Q�@���@��@���@�`B@�7L@��@��`@��@�Z@��@��@��y@���@��!@���@��\@�^5@�V@�=q@�-@�{@�J@��@�@��7@�p�@�X@�O�@�/@��`@�z�@��@�|�@��@��R@��+@�^5@�M�@�-@���@�x�@���@��j@���@�z�@��@���@��y@��#@��@�z�@�1@�|�@��@���@��\@�n�@�E�@��@�hs@�O�@�/@���@���@�Z@��@�S�@���@�^5@�-@��@���@�`B@�&�@��9@�r�@�(�@�  @���@�l�@�"�@��@��\@�^5@��@�@��#@��h@�%@�Ĝ@�z�@�Z@��@���@�@��\@�V@��@�?}@��`@��D@�Z@�ƨ@�l�@�33@��@���@�=q@�@��@���@��@�Q�@�9X@�1'@��;@�S�@��@���@���@�n�@��@��@�7L@��@���@��D@�bN@�(�@��@�\)@��y@���@�ff@�-@�{@��7@��@���@��@�I�@�A�@�(�@��;@�33@��H@�n�@�$�@��@���@�x�@��/@��j@�Q�@��@���@�S�@���@�@�@��-@���@���@���@���@���@���@���@��h@�`B@���@��D@�r�@�bN@�A�@�(�@��@�;@��@�@\)@�@�@~��@}@|�D@|�@|1@{�
@{33@z��@z~�@zn�@z^5@z=q@z�@zJ@y�@y��@x�`@x��@xr�@x �@x  @w\)@v�y@v�@vȴ@vȴ@v�R@v��@v��@v��@v��@vff@v5?@u�T@t�@t9X@t1@s�
@s�F@s��@st�@sS�@s"�@rM�@q�7@p�`@p �@o�@ol�@n�y@n��@nE�@m�@m@m�h@mO�@m?}@l�@ko@j�!@j��@j^5@i7L@hĜ@h�@hbN@h1'@g�@f��@fE�@f{@e�@e�@e�@d�j@dZ@d�@cdZ@cC�@c"�@b�H@b��@b�!@b-@a�#@a�^@aG�@`Q�@_\)@_\)@_
=@^�@^$�@]��@]�@]p�@]?}@\�@[ƨ@[dZ@Z^5@Yhs@X�`@Xr�@Xb@W��@W|�@W
=@V�R@U�T@U`B@T��@T�/@T�/@T��@T�j@Tz�@TI�@T(�@S��@Sƨ@S�@S"�@R��@Rn�@R�@Q��@QX@Q7L@PbN@N�y@Nv�@N@M�h@Mp�@MO�@M?}@M�@L��@L�/@L�/@L�@L�j@L�j@L�j@L��@Lz�@L(�@Ko@Jn�@JM�@J�@I�7@I%@HQ�@G�;@Gl�@F��@F�R@F��@F5?@F@E��@D��@D�@C��@Ct�@B�H@B-@A��@Ax�@Ahs@AG�@A7L@A&�@@bN@?+@>ff@=�T@=�T@=�T@=�T@=��@=�@<��@<j@<1@;S�@:��@:=q@9x�@9&�@8��@8��@8Ĝ@8��@8r�@8  @7�w@7\)@7K�@7;d@7+@7+@7�@7
=@7
=@6��@6�y@6�+@6ff@5@5�@5p�@5O�@5?}@5V@4�@4��@4z�@49X@3�
@3ƨ@3ƨ@3�F@3t�@333@3o@3o@2�@2��@2��@2��@2��@2~�@2^5@2=q@2J@2J@1�#@1G�@1�@0�`@0�u@0bN@0 �@0b@0  @/�;@/��@/+@.��@.V@.E�@-�@-��@-��@-�h@-`B@-/@-V@,�@,�@,9X@+�
@+�F@+��@+�@+"�@*��@*�!@*��@*~�@*n�@*^5@*=q@)��@(��@(��@'�@&�R@&ff@&V@&E�@&@&{@%�T@%�-@%�@%`B@%/@%�@$��@$Z@$I�@$1@#�m@#t�@#@"�H@"��@"=q@!�@!��@!hs@!hs@!G�@!%@ ��@ Ĝ@ �@ Q�@�@|�@\)@K�@�@��@ȴ@�R@��@E�@�@��@�-@�@`B@�/@��@z�@I�@I�@��@��@�@S�@"�@@�@��@^5@J@�7@�9@ �@�@�@
=@��@�+@ff@$�@@��@V@�/@��@j@Z@1@�
@ƨ@�@33@@��@�!@��@~�@^5@=q@�@��@��@��@hs@X@7L@7L@7L@&�@�@��@��@Ĝ@��@r�@bN@bN@1'@ �@�;@l�@l�111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߇+AߍPAߓuAߕ�Aߕ�Aߙ�Aߕ�Aߣ�A߬A߬Aߩ�A߬A߶FA߶FAߴ9A߮Aߣ�A�z�A�E�A�A�A�=qA�5?A��A�A��;A�K�A��`Aڲ-A�M�AӰ!A�~�A�Q�A���A�|�A�VA��-A��A�p�A��A�%A���A�v�A�9XA�z�A��A��9A���A��!A��FA�$�A�bNA��A�Q�A���A�(�A��7A�ZA��+A�jA��wA��PA��A�  A���A���A�JA��A�dZA� �A�ZA�
=A���A~��A{VAv5?AtJAsS�Aq�Alv�Ag7LAb(�A^�uA\�AW�ATI�AR�/AP5?AM��AJ�AG��AA��A?x�A=hsA<A�A;��A:�RA9�wA9VA8VA8JA7�
A7�wA7��A7|�A7�A5+A3|�A2�!A0A�A//A.9XA-"�A+�A*��A*�/A*ȴA*M�A)ƨA)"�A)%A(��A(�uA'�;A'dZA&�\A&$�A%��A$�A#&�A"�+A"{A!�#A!��A!ƨA!l�A ��A�#A��A�wA�A�jA1A �AƨAt�AA$�A`BAĜAZAO�A��Av�AZA(�AA��A�-AhsA�A��A9XA��A�mA��A�A�A�!A��A�DAz�A$�A��A�AA�+AjA�Ap�AG�A7LA&�A�HA�jAv�A=qA5?A{A�A�-AS�A;dA�A�RA{A�A�wA�AO�A��A�!AI�A1A�wA;dA
�A
9XA	�7A	\)A	O�A	33A	
=A	A�A�/A��A�jA�DAr�A=qA�A��A�FAp�A%A�An�A=qA�
AdZA��AĜA��A$�AAO�A��AȴAI�A�mA  A�^A"�A ĜA �!A ��A Q�@��F@�\)@���@��7@�/@�%@��j@��D@�Q�@�b@�l�@�n�@�X@��9@�ƨ@��H@��@�?}@��@�D@�@���@��@�;d@���@�E�@���@�@��@�bN@�w@�C�@�~�@��@�@���@�?}@�j@��@�9@��@ꗍ@���@�@� �@�(�@� �@�b@�;d@�5?@�hs@�G�@���@�@��@�\@�=q@��@���@�G�@�@ߥ�@�o@ާ�@ݩ�@ܴ9@�Q�@� �@�b@�1@�1@���@ۥ�@�S�@ڧ�@��T@�O�@���@�9X@�o@֧�@֧�@�@�Ĝ@�1'@�b@��;@ӶF@�dZ@��@ҧ�@�ff@�V@ϥ�@��@�ff@�J@���@�`B@�?}@��@�I�@��m@˝�@�\)@��y@�G�@��/@���@���@ȼj@ȼj@ȣ�@�z�@��
@�K�@Ƈ+@�{@�&�@�\)@�v�@��#@�O�@���@���@�9X@��;@��P@�
=@��R@��\@��@��-@���@�p�@�&�@��/@�j@��m@�|�@�+@��@�M�@��T@��7@�?}@��u@��@��w@��P@���@��\@�V@�{@�@�@�Q�@���@��@���@�`B@�7L@��@��`@��@�Z@��@��@��y@���@��!@���@��\@�^5@�V@�=q@�-@�{@�J@��@�@��7@�p�@�X@�O�@�/@��`@�z�@��@�|�@��@��R@��+@�^5@�M�@�-@���@�x�@���@��j@���@�z�@��@���@��y@��#@��@�z�@�1@�|�@��@���@��\@�n�@�E�@��@�hs@�O�@�/@���@���@�Z@��@�S�@���@�^5@�-@��@���@�`B@�&�@��9@�r�@�(�@�  @���@�l�@�"�@��@��\@�^5@��@�@��#@��h@�%@�Ĝ@�z�@�Z@��@���@�@��\@�V@��@�?}@��`@��D@�Z@�ƨ@�l�@�33@��@���@�=q@�@��@���@��@�Q�@�9X@�1'@��;@�S�@��@���@���@�n�@��@��@�7L@��@���@��D@�bN@�(�@��@�\)@��y@���@�ff@�-@�{@��7@��@���@��@�I�@�A�@�(�@��;@�33@��H@�n�@�$�@��@���@�x�@��/@��j@�Q�@��@���@�S�@���@�@�@��-@���@���@���@���@���@���@���@��h@�`B@���@��D@�r�@�bN@�A�@�(�@��@�;@��@�@\)@�@�@~��@}@|�D@|�@|1@{�
@{33@z��@z~�@zn�@z^5@z=q@z�@zJ@y�@y��@x�`@x��@xr�@x �@x  @w\)@v�y@v�@vȴ@vȴ@v�R@v��@v��@v��@v��@vff@v5?@u�T@t�@t9X@t1@s�
@s�F@s��@st�@sS�@s"�@rM�@q�7@p�`@p �@o�@ol�@n�y@n��@nE�@m�@m@m�h@mO�@m?}@l�@ko@j�!@j��@j^5@i7L@hĜ@h�@hbN@h1'@g�@f��@fE�@f{@e�@e�@e�@d�j@dZ@d�@cdZ@cC�@c"�@b�H@b��@b�!@b-@a�#@a�^@aG�@`Q�@_\)@_\)@_
=@^�@^$�@]��@]�@]p�@]?}@\�@[ƨ@[dZ@Z^5@Yhs@X�`@Xr�@Xb@W��@W|�@W
=@V�R@U�T@U`B@T��@T�/@T�/@T��@T�j@Tz�@TI�@T(�@S��@Sƨ@S�@S"�@R��@Rn�@R�@Q��@QX@Q7L@PbN@N�y@Nv�@N@M�h@Mp�@MO�@M?}@M�@L��@L�/@L�/@L�@L�j@L�j@L�j@L��@Lz�@L(�@Ko@Jn�@JM�@J�@I�7@I%@HQ�@G�;@Gl�@F��@F�R@F��@F5?@F@E��@D��@D�@C��@Ct�@B�H@B-@A��@Ax�@Ahs@AG�@A7L@A&�@@bN@?+@>ff@=�T@=�T@=�T@=�T@=��@=�@<��@<j@<1@;S�@:��@:=q@9x�@9&�@8��@8��@8Ĝ@8��@8r�@8  @7�w@7\)@7K�@7;d@7+@7+@7�@7
=@7
=@6��@6�y@6�+@6ff@5@5�@5p�@5O�@5?}@5V@4�@4��@4z�@49X@3�
@3ƨ@3ƨ@3�F@3t�@333@3o@3o@2�@2��@2��@2��@2��@2~�@2^5@2=q@2J@2J@1�#@1G�@1�@0�`@0�u@0bN@0 �@0b@0  @/�;@/��@/+@.��@.V@.E�@-�@-��@-��@-�h@-`B@-/@-V@,�@,�@,9X@+�
@+�F@+��@+�@+"�@*��@*�!@*��@*~�@*n�@*^5@*=q@)��@(��@(��@'�@&�R@&ff@&V@&E�@&@&{@%�T@%�-@%�@%`B@%/@%�@$��@$Z@$I�@$1@#�m@#t�@#@"�H@"��@"=q@!�@!��@!hs@!hs@!G�@!%@ ��@ Ĝ@ �@ Q�@�@|�@\)@K�@�@��@ȴ@�R@��@E�@�@��@�-@�@`B@�/@��@z�@I�@I�@��@��@�@S�@"�@@�@��@^5@J@�7@�9@ �@�@�@
=@��@�+@ff@$�@@��@V@�/@��@j@Z@1@�
@ƨ@�@33@@��@�!@��@~�@^5@=q@�@��@��@��@hs@X@7L@7L@7L@&�@�@��@��@Ĝ@��@r�@bN@bN@1'@ �@�;@l�@l�111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
?}B
?}B
?}B
?}B
>wB
?}B
?}B
@�B
@�B
@�B
A�B
K�B
dZB
jB
n�B
�B
�{B
�B
�dB
�jB
�wB
�wB
��B
ŢB
��B
��B
�BB
ÖB
�5B�B7LBB
�#By�B��B��B�/BhB+B&�B0!B%�B�B��BoBBBVB+B��B�B��B�B��B�VB�Bk�BT�BB�B8RB�B
�B
ɺB
ŢB
�1B
XB
ZB
A�B	��B	�B	��B	��B	��B	�B	�FB	��B	bNB	G�B	D�B	B�B	H�B	,B	?}B	^5B	L�B	B�B	O�B	YB	0!B	E�B	H�B	S�B	[#B	]/B	bNB	m�B	t�B	�+B	�VB	�bB	�VB	�=B	� B	m�B	iyB	l�B	]/B	ffB	^5B	^5B	jB	�B	��B	��B	��B	�!B	�jB	ǮB	��B	��B	�B	�mB	�B	��B	��B	��B	��B
1B
\B
oB
�B
{B
bB
bB
{B
(�B
-B
,B
'�B
,B
49B
2-B
5?B
49B
2-B
7LB
;dB
<jB
5?B
=qB
A�B
E�B
E�B
E�B
E�B
E�B
B�B
C�B
D�B
F�B
H�B
J�B
H�B
F�B
J�B
M�B
N�B
M�B
L�B
I�B
H�B
I�B
G�B
G�B
K�B
G�B
H�B
O�B
O�B
O�B
M�B
M�B
M�B
M�B
P�B
N�B
M�B
L�B
K�B
N�B
L�B
K�B
I�B
P�B
O�B
O�B
N�B
M�B
N�B
L�B
O�B
M�B
J�B
N�B
J�B
L�B
R�B
T�B
S�B
R�B
T�B
T�B
VB
VB
T�B
S�B
S�B
R�B
Q�B
Q�B
N�B
L�B
K�B
I�B
N�B
K�B
G�B
D�B
E�B
E�B
D�B
A�B
A�B
A�B
B�B
A�B
>wB
?}B
F�B
D�B
?}B
@�B
D�B
D�B
@�B
=qB
=qB
;dB
:^B
;dB
=qB
=qB
>wB
=qB
:^B
6FB
1'B
2-B
1'B
2-B
.B
/B
0!B
0!B
/B
0!B
2-B
/B
0!B
2-B
/B
0!B
.B
+B
)�B
'�B
'�B
$�B
%�B
'�B
&�B
%�B
#�B
%�B
'�B
 �B
�B
�B
�B
�B
#�B
#�B
"�B
�B
�B
 �B
$�B
!�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
uB
{B
uB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
oB
�B
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
�B
!�B
 �B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
%�B
$�B
#�B
"�B
"�B
#�B
#�B
'�B
'�B
(�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
(�B
(�B
(�B
'�B
%�B
%�B
$�B
%�B
%�B
(�B
(�B
)�B
)�B
(�B
'�B
'�B
&�B
(�B
)�B
(�B
&�B
&�B
%�B
"�B
&�B
(�B
)�B
)�B
,B
-B
.B
.B
.B
-B
,B
/B
/B
.B
-B
-B
,B
+B
,B
.B
0!B
/B
/B
0!B
0!B
/B
1'B
1'B
2-B
2-B
0!B
1'B
1'B
2-B
33B
2-B
33B
2-B
1'B
0!B
33B
33B
49B
2-B
33B
2-B
33B
49B
33B
33B
49B
5?B
6FB
5?B
6FB
7LB
7LB
7LB
6FB
8RB
8RB
7LB
5?B
6FB
:^B
:^B
9XB
7LB
;dB
;dB
:^B
:^B
:^B
;dB
<jB
>wB
>wB
?}B
?}B
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
?}B
>wB
C�B
B�B
B�B
D�B
C�B
B�B
A�B
C�B
B�B
D�B
D�B
E�B
C�B
B�B
D�B
D�B
C�B
E�B
D�B
C�B
D�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
I�B
I�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
K�B
J�B
K�B
M�B
N�B
N�B
M�B
M�B
O�B
P�B
P�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
P�B
O�B
P�B
O�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
O�B
N�B
Q�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
P�B
Q�B
R�B
S�B
T�B
VB
VB
VB
W
B
W
B
XB
XB
W
B
W
B
VB
S�B
YB
ZB
YB
W
B
YB
[#B
[#B
ZB
YB
W
B
[#B
\)B
\)B
[#B
[#B
[#B
[#B
\)B
[#B
]/B
]/B
]/B
^5B
]/B
\)B
]/B
^5B
]/B
[#B
\)B
_;B
_;B
`BB
_;B
_;B
aHB
`BB
`BB
_;B
]/B
_;B
^5B
_;B
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
e`B
ffB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
e`B
cTB
gmB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
iyB
iyB
gmB
hsB
jB
jB
hsB
hsB
hsB
iyB
iyB
jB
jB
k�B
jB
k�B
k�B
jB
k�B
n�B
m�B
k�B
k�B
l�B
m�B
n�B
n�B
m�B
l�B
jB
iyB
k�B
l�B
n�B
n�B
n�B
m�B
l�B
m�B
n�B
n�B
m�B
l�B
n�B
m�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
r�B
s�B
u�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
u�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
v�B
v�B
u�B
v�B
w�B
x�B
x�B
w�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|�B
}�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
}�B
}�B
{�B
z�B
{�B
{�B
{�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�%B
�%B
�B
�%B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�%B
�+B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�+B
�1B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�VB
�\B
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�hB
�uB
��111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
?}B
?}B
?}B
?}B
>wB
?}B
?}B
@�B
@�B
@�B
A�B
K�B
dZB
jB
n�B
�B
�{B
�B
�dB
�jB
�wB
�wB
��B
ŢB
��B
��B
�BB
ÖB
�5B�B7LBB
�#By�B��B��B�/BhB+B&�B0!B%�B�B��BoBBBVB+B��B�B��B�B��B�VB�Bk�BT�BB�B8RB�B
�B
ɺB
ŢB
�1B
XB
ZB
A�B	��B	�B	��B	��B	��B	�B	�FB	��B	bNB	G�B	D�B	B�B	H�B	,B	?}B	^5B	L�B	B�B	O�B	YB	0!B	E�B	H�B	S�B	[#B	]/B	bNB	m�B	t�B	�+B	�VB	�bB	�VB	�=B	� B	m�B	iyB	l�B	]/B	ffB	^5B	^5B	jB	�B	��B	��B	��B	�!B	�jB	ǮB	��B	��B	�B	�mB	�B	��B	��B	��B	��B
1B
\B
oB
�B
{B
bB
bB
{B
(�B
-B
,B
'�B
,B
49B
2-B
5?B
49B
2-B
7LB
;dB
<jB
5?B
=qB
A�B
E�B
E�B
E�B
E�B
E�B
B�B
C�B
D�B
F�B
H�B
J�B
H�B
F�B
J�B
M�B
N�B
M�B
L�B
I�B
H�B
I�B
G�B
G�B
K�B
G�B
H�B
O�B
O�B
O�B
M�B
M�B
M�B
M�B
P�B
N�B
M�B
L�B
K�B
N�B
L�B
K�B
I�B
P�B
O�B
O�B
N�B
M�B
N�B
L�B
O�B
M�B
J�B
N�B
J�B
L�B
R�B
T�B
S�B
R�B
T�B
T�B
VB
VB
T�B
S�B
S�B
R�B
Q�B
Q�B
N�B
L�B
K�B
I�B
N�B
K�B
G�B
D�B
E�B
E�B
D�B
A�B
A�B
A�B
B�B
A�B
>wB
?}B
F�B
D�B
?}B
@�B
D�B
D�B
@�B
=qB
=qB
;dB
:^B
;dB
=qB
=qB
>wB
=qB
:^B
6FB
1'B
2-B
1'B
2-B
.B
/B
0!B
0!B
/B
0!B
2-B
/B
0!B
2-B
/B
0!B
.B
+B
)�B
'�B
'�B
$�B
%�B
'�B
&�B
%�B
#�B
%�B
'�B
 �B
�B
�B
�B
�B
#�B
#�B
"�B
�B
�B
 �B
$�B
!�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
uB
{B
uB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
oB
�B
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
�B
!�B
 �B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
%�B
$�B
#�B
"�B
"�B
#�B
#�B
'�B
'�B
(�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
(�B
(�B
(�B
'�B
%�B
%�B
$�B
%�B
%�B
(�B
(�B
)�B
)�B
(�B
'�B
'�B
&�B
(�B
)�B
(�B
&�B
&�B
%�B
"�B
&�B
(�B
)�B
)�B
,B
-B
.B
.B
.B
-B
,B
/B
/B
.B
-B
-B
,B
+B
,B
.B
0!B
/B
/B
0!B
0!B
/B
1'B
1'B
2-B
2-B
0!B
1'B
1'B
2-B
33B
2-B
33B
2-B
1'B
0!B
33B
33B
49B
2-B
33B
2-B
33B
49B
33B
33B
49B
5?B
6FB
5?B
6FB
7LB
7LB
7LB
6FB
8RB
8RB
7LB
5?B
6FB
:^B
:^B
9XB
7LB
;dB
;dB
:^B
:^B
:^B
;dB
<jB
>wB
>wB
?}B
?}B
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
?}B
>wB
C�B
B�B
B�B
D�B
C�B
B�B
A�B
C�B
B�B
D�B
D�B
E�B
C�B
B�B
D�B
D�B
C�B
E�B
D�B
C�B
D�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
I�B
I�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
K�B
J�B
K�B
M�B
N�B
N�B
M�B
M�B
O�B
P�B
P�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
P�B
O�B
P�B
O�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
O�B
N�B
Q�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
P�B
Q�B
R�B
S�B
T�B
VB
VB
VB
W
B
W
B
XB
XB
W
B
W
B
VB
S�B
YB
ZB
YB
W
B
YB
[#B
[#B
ZB
YB
W
B
[#B
\)B
\)B
[#B
[#B
[#B
[#B
\)B
[#B
]/B
]/B
]/B
^5B
]/B
\)B
]/B
^5B
]/B
[#B
\)B
_;B
_;B
`BB
_;B
_;B
aHB
`BB
`BB
_;B
]/B
_;B
^5B
_;B
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
e`B
ffB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
e`B
cTB
gmB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
iyB
iyB
gmB
hsB
jB
jB
hsB
hsB
hsB
iyB
iyB
jB
jB
k�B
jB
k�B
k�B
jB
k�B
n�B
m�B
k�B
k�B
l�B
m�B
n�B
n�B
m�B
l�B
jB
iyB
k�B
l�B
n�B
n�B
n�B
m�B
l�B
m�B
n�B
n�B
m�B
l�B
n�B
m�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
s�B
r�B
s�B
u�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
u�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
v�B
v�B
u�B
v�B
w�B
x�B
x�B
w�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|�B
}�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
}�B
}�B
{�B
z�B
{�B
{�B
{�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�%B
�%B
�B
�%B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�%B
�+B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�+B
�1B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�VB
�\B
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�hB
�uB
��111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220903090121                              AO  ARCAADJP                                                                    20220903090121    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220903090121  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220903090121  QCF$                G�O�G�O�G�O�4000            