CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-17T00:35:13Z creation;2016-12-17T00:35:16Z conversion to V3.1;2019-12-19T08:23:00Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20161217003513  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA  I2_0576_068                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5�UU�1   @��6����@:ڬ��>B�d��x���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cr{Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�|)Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\Dۂ�Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��)D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��HA��`A��mA��mA��mA��A���A���A��A���A��A��A���A�  A���A���A�A�A�A�%A�  A�A�A�  A���A�A�A���A��RA���A���A��uA��PA��A�t�A�v�A�x�A�t�A�t�A�jA�VA�33A���A��A��+A��A�l�A��HA��A��uA��A��+A�A�{A�M�A��uA�|�A��HA��A�=qA��7A�ZA��A�"�A�VA�G�A�"�A��A���A�l�A�ffA�1A��\A��jA��wA��uA��;A�
=A�hsA���A�&�A�?}A�1'A��mA�v�A�x�A�t�A��A���A�G�A�M�A��RA�x�A���A��A�p�A�;dA���A��jA�hsA���A��7A�hsA�/A���A�I�A}�Ax��Av�AvA�At��As�-As�7Ar��Apr�Am�Ak�mAk�FAj�/Ai��AiAh1'Af��Ae��AdE�Ac�;Ac�wAc`BAb��Aa%A`�A_\)A^�yA^^5A]��A[�;AZ �AY
=AW�FAV�\ATĜAS33AR9XAQXAP�`AP=qAO�AO%AN=qAL��AL$�AKS�AK�AJ�RAJ �AI�FAH�AHAG��AGK�AFn�AF-AF1AEXAD�/AD �AB�jAA��AAC�A@�/A@�A>9XA=�PA=S�A=+A=VA<�A;A:5?A9O�A8�/A8�A8  A6�jA6A�A5�A4�A2��A1��A0ZA/dZA.�A.Q�A-A-"�A,bNA+l�A*�jA)�TA(^5A'�
A'�FA'��A'�A'p�A'x�A'x�A'+A&M�A%�PA#��A"�!A"A�A!��A!�A ��A�A`BA�A��A��AAAK�AVAQ�A|�A�A1AƨA�!A�mA�A�yA5?AC�AjA��A��A��A��A\)A
��A
��A
�/A
�A
�A	�A	K�A	"�A  A9XA��Al�A�+Ax�A
=A��AĜA�AdZA7LA �@�t�@�t�@���@�Z@��@�;d@�hs@�J@�j@�dZ@�@�x�@�7L@�V@�@�C�@�v�@�O�@��@�V@�%@��H@�ff@�$�@�7L@�bN@�ƨ@�
=@ݲ-@ܬ@��;@�o@�1@�C�@��T@�r�@�ff@ёh@�;d@��/@˥�@�v�@�z�@�1@�K�@��@Ƨ�@�@�ff@���@��@�E�@���@���@��@�  @�V@��@�hs@�G�@��@��;@�E�@�{@�~�@��!@���@�;d@�33@���@���@��9@�ƨ@�C�@��#@���@�O�@�`B@��u@��-@��/@���@�-@��#@�x�@��/@�r�@��
@�v�@�x�@�;d@���@���@�bN@��F@�dZ@��!@��T@��@���@�9X@��w@�\)@���@�E�@�5?@�J@�{@��#@�z�@���@�;d@��@��H@���@��!@�~�@�=q@��#@�`B@�X@�O�@�O�@��j@��
@�S�@���@��D@�1@���@�l�@�"�@�v�@��T@���@�`B@�?}@�&�@�%@���@��@�j@��@���@��F@���@�l�@�S�@�;d@��@��!@���@��+@�V@�{@��@���@��h@�7L@��u@�  @��H@���@��R@���@��\@�-@��@��/@�r�@� �@�  @�ƨ@��@�l�@�@��R@�n�@�-@�J@���@��T@���@�x�@�?}@�7L@�7L@�7L@�/@�&�@���@�z�@��D@��u@��u@�j@�b@�@�@~�+@}��@}�-@}�h@}�@|z�@|�@|1@{��@{�
@{�@{S�@{33@{o@z�H@z^5@y��@y�7@y�@xĜ@x��@x�u@xr�@xr�@xQ�@x1'@w��@v��@v��@vv�@vE�@v5?@v@u�T@u�h@t��@tj@tz�@t�@t��@t��@u?}@uO�@u�h@u��@t�j@t9X@tZ@tZ@t9X@s�m@s�F@sdZ@so@r�@r��@r�\@rn�@r�@rJ@p��@o��@o��@oK�@n��@n�@n�@n�@n�y@n�y@nȴ@n�@n��@n�y@m�@m/@m�@l�j@lj@l�@l1@k�m@k�@kS�@j��@j=q@jJ@i�#@i�^@i��@i��@hb@g\)@g�@g;d@f�R@f��@fV@f@e�T@e�-@e@e@d��@dj@d(�@cƨ@c�@c@b�\@b^5@`�9@`1'@`1'@`  @_�@^��@^�@^E�@]�-@]/@\��@\��@\�/@\��@\�@\Z@\Z@\��@\1@Z~�@Y��@Yx�@YX@Z=q@Z��@[@Z�@Z��@Z~�@Y��@Y&�@X�`@X�u@XQ�@W�w@W�P@W|�@W;d@V��@V{@U�T@U�@U/@T�/@T�j@T�D@S��@S@R�H@R�\@R^5@RM�@Q��@Q�^@Q�7@Q�@P��@P�`@P��@PĜ@P1'@O��@O\)@O;d@O+@O
=@N�@Nȴ@Nv�@Nff@NE�@N$�@N$�@N@M�-@M/@L��@Lj@Lj@LI�@L�@K�
@K��@KS�@J��@I��@Ihs@I&�@H�u@HA�@H �@H  @G�@G��@G�w@G�P@GK�@G�@F�R@E�@EO�@D�/@D�D@Dj@DZ@DI�@D(�@C��@C�m@C�m@C�
@C��@Ct�@B~�@A��@A��@@��@@�@@Q�@@1'@@ �@@b@?�;@?�P@>ȴ@>@=��@=p�@=O�@=/@=�@<��@<Z@;��@;"�@:�H@:�!@:n�@:=q@:-@9�#@9X@9&�@8Ĝ@8�@8Q�@8Q�@8A�@7�@7�P@7�P@7�P@7|�@7K�@7K�@7
=@6�y@6��@6V@5�T@4�@41@3�F@3dZ@3S�@3S�@3S�@3S�@3C�@333@3o@2�@2��@2�\@2^5@2-@1��@1G�@0�@0  @/�@/�@/�@/�P@/�P@/l�@/+@.ȴ@.v�@.$�@-@-@-`B@-V@,�@,�@,Z@,(�@+�
@+"�@*�@*�\@*J@)��@)&�@(bN@(b@'��@'�@'�P@'K�@'�@&��@&ȴ@&�+@&$�@%�@%��@%p�@$��@$�D@$Z@$�@#��@#�
@#C�@"��@"n�@!�@!��@!hs@!�@!%@ Ĝ@ �u@ r�@ 1'@ 1'@�@��@�@|�@l�@K�@;d@+@�@�@
=@�y@�R@��@�+@ff@V@5?@5?@{@�@�h@`B@?}@V@�@�D@(�@1@��@�
@ƨ@��@�@t�@dZ@"�@@@�H@��@�!@��@~�@n�@M�@-@��@�^@x�@G�@7L@&�@%@�9@�u@A�@\)@ȴ@�+@V@E�@5?@{@@�@�T@�T@�h@O�@�@��@Z@9X@�@1@�m@�
@�
@�m@ƨ@�F@�@C�@��@n�@=q@J@�@�^@�7@7L@�@��@�9@��@r�@ �@�;@��@�@|�@\)@K�@;d@+@+@�@
=@�y@�R@�R@��@��@ff@E�@{@@��@O�@V@�/@�j@�j@�@�@�@��@�D@z�@j@Z@I�@(�@�@�@�m@��@t�@33@o@o@o@"�@
�!@
~�@
n�@
n�@
^5@
^5@
J@	�7@	G�@	&�@	�@��@��@�u@r�@A�@  @�@\)@�@�@v�@�T@@�h@�h@�h@p�@?}@�/@z�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��HA��`A��mA��mA��mA��A���A���A��A���A��A��A���A�  A���A���A�A�A�A�%A�  A�A�A�  A���A�A�A���A��RA���A���A��uA��PA��A�t�A�v�A�x�A�t�A�t�A�jA�VA�33A���A��A��+A��A�l�A��HA��A��uA��A��+A�A�{A�M�A��uA�|�A��HA��A�=qA��7A�ZA��A�"�A�VA�G�A�"�A��A���A�l�A�ffA�1A��\A��jA��wA��uA��;A�
=A�hsA���A�&�A�?}A�1'A��mA�v�A�x�A�t�A��A���A�G�A�M�A��RA�x�A���A��A�p�A�;dA���A��jA�hsA���A��7A�hsA�/A���A�I�A}�Ax��Av�AvA�At��As�-As�7Ar��Apr�Am�Ak�mAk�FAj�/Ai��AiAh1'Af��Ae��AdE�Ac�;Ac�wAc`BAb��Aa%A`�A_\)A^�yA^^5A]��A[�;AZ �AY
=AW�FAV�\ATĜAS33AR9XAQXAP�`AP=qAO�AO%AN=qAL��AL$�AKS�AK�AJ�RAJ �AI�FAH�AHAG��AGK�AFn�AF-AF1AEXAD�/AD �AB�jAA��AAC�A@�/A@�A>9XA=�PA=S�A=+A=VA<�A;A:5?A9O�A8�/A8�A8  A6�jA6A�A5�A4�A2��A1��A0ZA/dZA.�A.Q�A-A-"�A,bNA+l�A*�jA)�TA(^5A'�
A'�FA'��A'�A'p�A'x�A'x�A'+A&M�A%�PA#��A"�!A"A�A!��A!�A ��A�A`BA�A��A��AAAK�AVAQ�A|�A�A1AƨA�!A�mA�A�yA5?AC�AjA��A��A��A��A\)A
��A
��A
�/A
�A
�A	�A	K�A	"�A  A9XA��Al�A�+Ax�A
=A��AĜA�AdZA7LA �@�t�@�t�@���@�Z@��@�;d@�hs@�J@�j@�dZ@�@�x�@�7L@�V@�@�C�@�v�@�O�@��@�V@�%@��H@�ff@�$�@�7L@�bN@�ƨ@�
=@ݲ-@ܬ@��;@�o@�1@�C�@��T@�r�@�ff@ёh@�;d@��/@˥�@�v�@�z�@�1@�K�@��@Ƨ�@�@�ff@���@��@�E�@���@���@��@�  @�V@��@�hs@�G�@��@��;@�E�@�{@�~�@��!@���@�;d@�33@���@���@��9@�ƨ@�C�@��#@���@�O�@�`B@��u@��-@��/@���@�-@��#@�x�@��/@�r�@��
@�v�@�x�@�;d@���@���@�bN@��F@�dZ@��!@��T@��@���@�9X@��w@�\)@���@�E�@�5?@�J@�{@��#@�z�@���@�;d@��@��H@���@��!@�~�@�=q@��#@�`B@�X@�O�@�O�@��j@��
@�S�@���@��D@�1@���@�l�@�"�@�v�@��T@���@�`B@�?}@�&�@�%@���@��@�j@��@���@��F@���@�l�@�S�@�;d@��@��!@���@��+@�V@�{@��@���@��h@�7L@��u@�  @��H@���@��R@���@��\@�-@��@��/@�r�@� �@�  @�ƨ@��@�l�@�@��R@�n�@�-@�J@���@��T@���@�x�@�?}@�7L@�7L@�7L@�/@�&�@���@�z�@��D@��u@��u@�j@�b@�@�@~�+@}��@}�-@}�h@}�@|z�@|�@|1@{��@{�
@{�@{S�@{33@{o@z�H@z^5@y��@y�7@y�@xĜ@x��@x�u@xr�@xr�@xQ�@x1'@w��@v��@v��@vv�@vE�@v5?@v@u�T@u�h@t��@tj@tz�@t�@t��@t��@u?}@uO�@u�h@u��@t�j@t9X@tZ@tZ@t9X@s�m@s�F@sdZ@so@r�@r��@r�\@rn�@r�@rJ@p��@o��@o��@oK�@n��@n�@n�@n�@n�y@n�y@nȴ@n�@n��@n�y@m�@m/@m�@l�j@lj@l�@l1@k�m@k�@kS�@j��@j=q@jJ@i�#@i�^@i��@i��@hb@g\)@g�@g;d@f�R@f��@fV@f@e�T@e�-@e@e@d��@dj@d(�@cƨ@c�@c@b�\@b^5@`�9@`1'@`1'@`  @_�@^��@^�@^E�@]�-@]/@\��@\��@\�/@\��@\�@\Z@\Z@\��@\1@Z~�@Y��@Yx�@YX@Z=q@Z��@[@Z�@Z��@Z~�@Y��@Y&�@X�`@X�u@XQ�@W�w@W�P@W|�@W;d@V��@V{@U�T@U�@U/@T�/@T�j@T�D@S��@S@R�H@R�\@R^5@RM�@Q��@Q�^@Q�7@Q�@P��@P�`@P��@PĜ@P1'@O��@O\)@O;d@O+@O
=@N�@Nȴ@Nv�@Nff@NE�@N$�@N$�@N@M�-@M/@L��@Lj@Lj@LI�@L�@K�
@K��@KS�@J��@I��@Ihs@I&�@H�u@HA�@H �@H  @G�@G��@G�w@G�P@GK�@G�@F�R@E�@EO�@D�/@D�D@Dj@DZ@DI�@D(�@C��@C�m@C�m@C�
@C��@Ct�@B~�@A��@A��@@��@@�@@Q�@@1'@@ �@@b@?�;@?�P@>ȴ@>@=��@=p�@=O�@=/@=�@<��@<Z@;��@;"�@:�H@:�!@:n�@:=q@:-@9�#@9X@9&�@8Ĝ@8�@8Q�@8Q�@8A�@7�@7�P@7�P@7�P@7|�@7K�@7K�@7
=@6�y@6��@6V@5�T@4�@41@3�F@3dZ@3S�@3S�@3S�@3S�@3C�@333@3o@2�@2��@2�\@2^5@2-@1��@1G�@0�@0  @/�@/�@/�@/�P@/�P@/l�@/+@.ȴ@.v�@.$�@-@-@-`B@-V@,�@,�@,Z@,(�@+�
@+"�@*�@*�\@*J@)��@)&�@(bN@(b@'��@'�@'�P@'K�@'�@&��@&ȴ@&�+@&$�@%�@%��@%p�@$��@$�D@$Z@$�@#��@#�
@#C�@"��@"n�@!�@!��@!hs@!�@!%@ Ĝ@ �u@ r�@ 1'@ 1'@�@��@�@|�@l�@K�@;d@+@�@�@
=@�y@�R@��@�+@ff@V@5?@5?@{@�@�h@`B@?}@V@�@�D@(�@1@��@�
@ƨ@��@�@t�@dZ@"�@@@�H@��@�!@��@~�@n�@M�@-@��@�^@x�@G�@7L@&�@%@�9@�u@A�@\)@ȴ@�+@V@E�@5?@{@@�@�T@�T@�h@O�@�@��@Z@9X@�@1@�m@�
@�
@�m@ƨ@�F@�@C�@��@n�@=q@J@�@�^@�7@7L@�@��@�9@��@r�@ �@�;@��@�@|�@\)@K�@;d@+@+@�@
=@�y@�R@�R@��@��@ff@E�@{@@��@O�@V@�/@�j@�j@�@�@�@��@�D@z�@j@Z@I�@(�@�@�@�m@��@t�@33@o@o@o@"�@
�!@
~�@
n�@
n�@
^5@
^5@
J@	�7@	G�@	&�@	�@��@��@�u@r�@A�@  @�@\)@�@�@v�@�T@@�h@�h@�h@p�@?}@�/@z�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BiyBiyBiyBjBjBiyBiyBiyBiyBiyBiyBiyBiyBhsBiyBiyBhsBhsBhsBhsBiyBhsBiyBiyBiyBiyBiyBiyBp�Bq�Bq�Br�Bs�Bt�Bu�Bu�Bt�Bu�Bu�Bv�By�B}�B�B�PB�bBe`B0!B��B��B��B��B��B�B�/B��B��B��B�uB�+BhsBF�BB�B=qB1'B'�B�B�B�B\BB�B�B�B��BƨB�XB��B�bB�%B|�Br�BffBYBT�BXBP�BC�B<jB5?B-B�BhB
��B
�B
�yB
�BB
�)B
�#B
ǮB
B
�3B
�B
�B
��B
��B
��B
�B
VB
G�B
P�B
C�B
;dB
<jB
;dB
2-B
�B
PB
PB
hB
DB
%B
B	��B	�B	�fB	�HB	�;B	�/B	�B	��B	ǮB	ÖB	��B	�}B	�^B	�!B	��B	��B	��B	�\B	�B	z�B	s�B	o�B	k�B	hsB	dZB	aHB	]/B	VB	P�B	Q�B	R�B	Q�B	O�B	L�B	I�B	C�B	@�B	?}B	;dB	9XB	9XB	6FB	33B	1'B	)�B	$�B	!�B	�B	�B	hB	VB	JB	JB	DB	DB	1B��B��B��B�B��B�B�mB�/B��BƨB�}B�jB�^B�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�PB�JB�DB�B�B}�Bz�Bx�Bw�Bt�Br�Bp�Bo�Bl�BgmBdZBaHB]/B[#BXBVBQ�BN�BM�BM�BJ�BJ�BJ�BJ�BJ�BK�BI�BH�BH�BC�B@�BA�B@�B>wB=qB<jB<jB<jB;dB:^B:^B;dB8RB6FB33B1'B0!B1'B,B+B)�B,B+B)�B)�B(�B(�B'�B%�B'�B&�B&�B%�B#�B$�B%�B&�B'�B(�B)�B(�B&�B&�B#�B#�B&�B(�B+B+B/B,B&�B'�B"�B!�B�B�B�B"�B"�B�B�B�B�B#�B%�B%�B)�B(�B'�B'�B'�B)�B,B0!B6FB:^B>wBC�BE�BD�BC�BE�BG�BK�BL�BM�BVBYB\)BZB\)BZB]/B_;BaHBaHBbNBdZBdZBdZBdZBdZBhsBl�Bm�Bm�Bp�Bq�Bp�Bq�Bv�B~�B�B�=B�uB�uB��B��B��B��B��B��B��B��B��B��B�B�!B�9B�RB�XB�jB�wB��BÖBB�wB��BBŢBƨBɺB��B��B��B��B�B�
B�B�B�B�)B�5B�BB�NB�ZB�fB�fB�mB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	1B	\B	uB	�B	�B	�B	 �B	%�B	,B	0!B	33B	49B	5?B	9XB	;dB	<jB	<jB	<jB	<jB	<jB	<jB	=qB	B�B	E�B	E�B	F�B	G�B	F�B	G�B	I�B	J�B	L�B	L�B	L�B	N�B	Q�B	S�B	S�B	T�B	VB	XB	YB	ZB	[#B	]/B	`BB	bNB	e`B	hsB	iyB	jB	jB	k�B	k�B	l�B	m�B	o�B	t�B	x�B	y�B	z�B	z�B	{�B	|�B	� B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�FB	�LB	�dB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ĜB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
1B
	7B
1B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
\B
bB
\B
\B
bB
bB
hB
hB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BiyBiyBiyBjBjeBiyBiyBiyBiyBiyBiyBiyBiyBhsBiyBi_BhsBhsBhsBh�BiyBhsBiyBiyBiyBi�Bi�Bi�Bp�Bq�Bq�Br�Bs�Bt�Bu�Bu�Bt�Bu�Bu�BwBzxBB��B��B�Bl=B4nB��B�0B�*B�fB��B�B�&BÖB��B�B��B�<Bk�BG�BC�B?cB3hB)_B=BBYB�B�B��B�B�WB�hBɠB��B��B�B��B~]Bt�BhsBZBV�BZkBR�BD�B=�B6zB/5B �B�B
��B
�MB
��B
�B
�dB
ݘB
�7B
�mB
��B
��B
��B
�B
�XB
��B
�tB
X_B
H�B
RoB
D�B
<B
=�B
>(B
4�B
 �B
�B
�B
�B
JB
zB
�B	�+B	��B	��B	�B	��B	�5B	�=B	� B	ȚB	�gB	�oB	��B	��B	�-B	�sB	�vB	�EB	��B	��B	|B	t�B	pUB	lqB	iDB	e`B	b�B	^�B	W$B	Q�B	RTB	S�B	R�B	P�B	M�B	J�B	D3B	A;B	@iB	;�B	9�B	:DB	7B	4nB	2�B	+B	%�B	"�B	B	�B	:B	�B	�B	�B	�B	�B		�B�B�fB�ZB��B�+B�B��B޸BҽB��B�UB��B�0B�B��B�AB�5B�]B�B�KB��B�bB�B��B��B��B��B�:B��B�$B�8B��B��B�YB�kB�YB�FB�uB�HB�pB�"B�PB��B�uB~�B{By�By	Bu�BshBqvBp�Bm�Bh$BeFBbhB^�B\xBYBX+BSBO�BOBBNpBJ�BJ�BJ�BKDBK�BL�BJ�BJrBJ�BDMBAUBB�BA�B?B=�B="B=�B=<B<B;dB<6B=�B9�B72B3�B1�B1�B3B-B,B*�B,qB+QB*KB*B)�B)�B(�B'B)B(
B($B&LB$@B%�B&�B'�B(�B)�B*�B)�B'�B(�B$�B$�B($B*KB,B,�B0�B,�B(
B)B#TB"hB BB BB�B$�B$B�B \B 'B BB$�B&�B&�B*B)DB(XB(�B(�B*�B,=B0B6+B:DB>�BC�BE�BE9BD�BFYBHKBL�BMPBM�BVSBZB]�B[	B]dBZ�B]~B_�Ba�Ba�BcBe`Be`Be�Bd�Be,BiDBmBnBn/BqABr-Bq'BrGBw2BHB��B��B��B��B��B�B�yB�#B�B��B��B�B�B�*B�QB�oB��B�lB�rB��B��B�AB�MBðB�HB�B��B��B�B�=B�HB�:B�FB�2B�B�YB�_B�KB�kB�xBބB�\B�B�B�B�B�B�B�B�B��B��B��B��B��B�%B�LB�XB�B��B��B�B�<B�]B�}B	�B	mB	�B	�B	�B	�B	�B	�B	!B	&B	,=B	0;B	3MB	4nB	5tB	9rB	;�B	<�B	<�B	<�B	<�B	<�B	<�B	=�B	B�B	E�B	E�B	F�B	G�B	F�B	HB	I�B	KB	L�B	L�B	MB	OB	RB	TB	TB	UB	VB	XEB	Y1B	ZQB	[=B	]dB	`vB	b�B	e�B	h�B	i�B	j�B	j�B	k�B	k�B	l�B	m�B	pB	t�B	y	B	y�B	z�B	z�B	|B	}"B	�OB	�+B	�7B	�DB	�JB	�PB	�pB	�\B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	��B	�|B	�MB	�`B	��B	�B	�wB	��B	��B	�}B	��B	��B	ÖB	ĶB	�B	��B	��B	�B	� B	� B	��B	� B	�B	� B	�4B	� B	�B	�B	�B	�B	�,B	ՁB	�?B	�
B	�KB	�QB	�OB	�jB	�VB	�VB	�\B	�bB	�hB	�B	�zB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	�B	��B	�B	��B	�B	�+B	��B	��B	�B	�vB	��B	��B	��B	��B	��B	�$B	�$B	��B	��B	�	B	�B	��B	��B	�B	�B	�"B	�(B	�(B	�B	�B
 B
 4B
 OB
GB
3B
3B
9B
?B
YB
_B
EB
_B
fB
	7B
KB
	RB
fB

rB
^B
^B
^B
dB
dB
dB
dB
jB
jB
pB
VB
�B
�B
�B
�B
}B
\B
vB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B
 �B
!�B
"B
#�B
#�B
#�B
$�B
$�B
$�B
%B
%,B
&2B
'B
(
B
(
B
(
B
(
B
($B
)*B
*eB
+6B
+B
,=B
,"B
,"B
,"B
-)B
-CB
./B
.IB
/5B
/5B
/B
/5B
/5B
/5B
0!B
0!B
0UB
0;B
0!B
0;B
0;B
1AB
1[B
1�B
2�B
5tB
6`B
6`B
7LB
7fB
7LB
7LB
7fB
7fB
7fB
7fB
7fB
7fB
8�B
8�B
8�B
8�B
9�B
:�B
;dB
;B
;B
;B
;dB
;B
;B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
LB
L�B
MB
M�B
NB
M�B
N�B
N�B
OB
OB
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q B
P�B
P�B
P�B
P�B
P�B
QB
R B
RB
RB
RB
R�B
SB
R�B
SB
SB
SB
TB
TB
T,B
T,B
T,B
UB
UB
VB
VB
VB
VB
VB
VB
VB
W?B
W$B
W$B
W$B
W$B
W$B
W
B
W$B
XB
XEB
X+B
X+B
X+B
YKB
Y1B
YB
Y1B
Y1B
Y1B
Z7B
ZQB
ZkB
\]B
\]B
\CB
]/B
]IB
]IB
]/B
]/B
]/B
]IB
]IB
^OB
^OB
^jB
_VB
_VB
_VB
_VB
_VB
_;B
`BB
`BB
_VB
_VB
`\B
`\B
`vB
`\B
abB
abB
abB
a|B
a|B
bhB
bhB
bhB
bhB
bhB
c�B
c�B
cnB
cTB
cnB
cnB
d�B
dZB
dZB
dZB
dtB
dZB
dZB
dtB
dtB
dZB
dZB
d�B
dtB
ezB
ezB
f�B
f�B
f�B
f�B
f�B
gmB
gmB
gmB
ffB
gRB
f�B
g�B
ffB
gmB
gmB
gRB
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
hsB
hsB
hsB
h�B
h�B
hsB
h�B
hsB
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612210035432016122100354320161221003543201806221218222018062212182220180622121822201804050411292018040504112920180405041129  JA  ARFMdecpA19c                                                                20161217093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161217003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161217003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161217003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161217003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161217003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161217003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161217003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161217003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161217003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20161217013315                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161217153530  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161220153543  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161220153543  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191129  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031822  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                