CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-22T00:35:42Z creation;2018-06-22T00:35:47Z conversion to V3.1;2019-12-19T07:35:20Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180622003542  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_253                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�l6#u� 1   @�l6��J @4��p:��dR�d��81   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @%�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�|)DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�B�D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��D�E�D�b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�1'A��A�p�A�"�AӅA�$�A�S�A�dZA���A�XA��
A���AϋDA��A�t�A�{A�bNAˋDA���A�AǃA��A���A��#A�A�;dA�`BA�-A��uA��jA��TA��A�I�A�\)A�(�A��\A��\A��A�VA�A��^A���A�~�A���A�G�A��A�ZA�1'A�"�A���A� �A�ĜA���A�K�A��A��A�r�A��A���A���A��7A�\)A���A�&�A���A�|�A��
A��A��^A�  A���A��HA�ȴA��
A�l�A���A���A��A��
A��A�ƨA��yA�$�A�1'A���A�"�A�x�A���A�\)A�n�A�n�A��A�r�A�"�A���At�A~  A}/A|ĜA{��Az�!Ayt�Ax��Aw;dAvJAtȴAsK�Aq��Ap��Ao�Am��Al�!Ak�^AjȴAi��AgK�Ad-Ac�Ac�A_"�A\�`A[�AX��AVbNAUO�AU/AT��AR�!AP��AP{AN�RAK�AIK�AGK�AF�AE�ABz�AA�A>��A<(�A9hsA8ZA7O�A6JA5K�A4�A3��A2bNA1�A1&�A/��A.ZA-C�A,  A+��A)�mA)�A(�uA'l�A&�A#�TA"�jA"r�A!x�A n�A��A  A��A7LA�A�+A��A\)AȴA��A�PA�A��A-A�hAE�AƨA?}A��A5?A�A�A��A�A��AM�A��A
��A
�A	��A	C�AZAƨAC�A9XAM�A?}AĜAM�A�A b@��@��@�X@�9X@�|�@��y@�M�@�O�@�t�@��y@�$�@�&�@��@�+@�$�@�?}@�9X@�"�@��@�%@�u@�P@�v�@�hs@�@��@�j@�t�@�V@�@�G�@�?}@���@�Z@�1@�dZ@�hs@֏\@��T@���@ӍP@ҟ�@�M�@��#@�?}@��`@�j@�"�@��@͙�@̣�@��@�-@�O�@ȋD@ǝ�@�C�@�{@�r�@�@�V@� �@�=q@�&�@�I�@���@�\)@��@���@�@�/@���@�j@�  @��F@���@�^5@�G�@���@��@��D@�bN@�1@��m@��P@���@���@��R@�^5@��@���@�x�@�O�@��@���@� �@�ƨ@�C�@�
=@��H@�^5@��@�@�`B@��@���@��@��F@�t�@�o@��\@�ff@�$�@��@��#@���@�x�@�7L@�Z@�1@�1'@���@�S�@��y@���@�v�@�M�@���@��7@�hs@�X@�&�@�V@��@��D@�r�@��
@�t�@���@��@��w@���@�K�@�@��R@�~�@��^@�&�@��`@���@�(�@���@��@�S�@�^5@�=q@�@�$�@��@�J@���@��@�/@�bN@��@��;@��w@��w@�ƨ@��@�|�@�S�@�C�@�ȴ@���@�~�@�ff@�{@��@�O�@�`B@�`B@��@���@�(�@�ƨ@�;d@�C�@���@�
=@��\@�5?@�-@��@�J@���@�%@��D@�I�@�A�@��@��F@��@��;@�dZ@���@��+@�5?@��-@�O�@��@���@��9@���@��@�Z@� �@�  @��F@��P@�|�@�33@��@���@�v�@�M�@�J@��#@�hs@�&�@�V@��@��@�Ĝ@��@�9X@�1'@�b@�ƨ@���@�l�@�
=@��@���@�~�@�=q@�@��@��@�$�@��7@�`B@�G�@�?}@�?}@�7L@�7L@�&�@��@��/@�A�@�9X@�(�@�b@���@���@���@��@�+@��R@�^5@��T@�x�@��@���@�(�@��m@�|�@�\)@�K�@�o@��@��@���@��+@�ff@�M�@�5?@��7@��@��@���@���@�Ĝ@�bN@�Z@�Q�@�9X@���@��9@�(�@K�@~��@}��@}��@}��@}`B@}/@|�D@|9X@|1@{ƨ@{�F@{33@z��@z��@z��@z��@y��@x1'@w��@u��@t��@tj@tZ@s��@s@r��@r��@r��@r��@s@r^5@q�^@q�#@q��@q��@q&�@p��@p�9@p�@p �@nV@m�h@k�F@k�@kS�@j�\@j~�@j�!@kt�@kS�@kS�@j��@jM�@i7L@h��@h�9@hb@g��@g�w@g��@g
=@fv�@f5?@fE�@f{@e�h@e/@d��@d��@dI�@d1@cƨ@cdZ@b�!@a�@a�^@a��@aX@a%@`��@`�9@`�9@`r�@` �@_��@_�@^ȴ@^�+@^ff@^V@^E�@^E�@]�@]p�@]`B@]O�@\��@\�D@\Z@[ƨ@Z�@Z�H@Z��@Z�\@Zn�@Zn�@Zn�@Zn�@Zn�@Z^5@Z^5@Z-@Y�@Y��@Yhs@Y&�@X�u@Xr�@XbN@XQ�@X1'@W�@W;d@V�y@V��@Vff@V{@U��@U`B@UO�@T��@T�/@T�D@S�
@SS�@S"�@So@R�@R��@R^5@Q�^@Q7L@Qx�@QG�@P�u@O�w@OK�@O+@N�y@Nȴ@N��@N�+@NV@NE�@NE�@N$�@M�T@Mp�@L��@L�/@L��@L�@L(�@K�m@K��@KdZ@K33@K"�@J�@J�H@J��@J�!@JJ@I��@Ihs@H��@HA�@Hb@G�P@G;d@F�@F��@FE�@F{@F@F@E�-@Ep�@E/@D��@D�D@D1@Cƨ@C�@CdZ@B��@B^5@B=q@A�@A�7@A�@@�`@@�u@@Q�@@A�@@1'@?�@?�w@?�@?��@?�P@?|�@?+@>�y@>��@=�T@=/@<�/@<z�@<z�@<j@<Z@<1@;��@:��@:�!@:~�@:^5@9�@9�^@9x�@9X@9%@8��@8 �@7��@7|�@7K�@7+@7
=@6��@6E�@65?@5�@5@5�@5O�@5/@4�@4�j@4��@49X@4(�@3��@3ƨ@3�F@3��@3�@3S�@3C�@3o@2�H@2��@2n�@2=q@2J@1��@1��@1&�@0�`@0�9@0r�@0bN@0bN@0Q�@0A�@/�@/��@/�P@/K�@/+@/
=@.�R@.�+@.@-��@-`B@-O�@-�@,�@,�/@,��@,�j@,��@,��@,j@,I�@,(�@+��@+��@+S�@+@*�!@*�\@*-@)�^@)hs@(Ĝ@(��@(�@(bN@'�;@'|�@'\)@'l�@'K�@'
=@&��@&�+@&5?@&{@&@%�@%�T@%��@%�@%/@$��@$��@$��@$z�@$Z@$Z@$9X@#�m@#��@#��@#t�@#S�@#"�@"��@"�\@"�@!�^@!�^@!��@!X@ �`@ r�@  �@�@��@��@l�@ȴ@��@��@��@�+@E�@�@��@�h@�@�@O�@��@�j@�j@�@j@�m@ƨ@��@C�@��@��@~�@M�@�@��@�7@�@Ĝ@�u@A�@  @�w@�P@\)@K�@�@ȴ@�R@�+@V@5?@@�T@�-@�@V@��@�D@z�@�@�F@��@dZ@S�@C�@"�@�@�!@n�@M�@=q@��@��@x�@G�@%@��@�u@r�@Q�@Q�@1'@b@  @�;@��@\)@;d@�@��@�y@ȴ@��@�+@ff@5?@�@@@�-@�-@�@`B@O�@V@�j@��@�D@Z@I�@I�@(�@�@��@�
@�
@ƨ@��@dZ@S�@C�@o@
��@
�\@
~�@
n�@
=q@
-@
�@
J@	��@	��@	�@	�#@	�^@	��@	�7@	x�@	X@	&�@	�@�`@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�1'A��A�p�A�"�AӅA�$�A�S�A�dZA���A�XA��
A���AϋDA��A�t�A�{A�bNAˋDA���A�AǃA��A���A��#A�A�;dA�`BA�-A��uA��jA��TA��A�I�A�\)A�(�A��\A��\A��A�VA�A��^A���A�~�A���A�G�A��A�ZA�1'A�"�A���A� �A�ĜA���A�K�A��A��A�r�A��A���A���A��7A�\)A���A�&�A���A�|�A��
A��A��^A�  A���A��HA�ȴA��
A�l�A���A���A��A��
A��A�ƨA��yA�$�A�1'A���A�"�A�x�A���A�\)A�n�A�n�A��A�r�A�"�A���At�A~  A}/A|ĜA{��Az�!Ayt�Ax��Aw;dAvJAtȴAsK�Aq��Ap��Ao�Am��Al�!Ak�^AjȴAi��AgK�Ad-Ac�Ac�A_"�A\�`A[�AX��AVbNAUO�AU/AT��AR�!AP��AP{AN�RAK�AIK�AGK�AF�AE�ABz�AA�A>��A<(�A9hsA8ZA7O�A6JA5K�A4�A3��A2bNA1�A1&�A/��A.ZA-C�A,  A+��A)�mA)�A(�uA'l�A&�A#�TA"�jA"r�A!x�A n�A��A  A��A7LA�A�+A��A\)AȴA��A�PA�A��A-A�hAE�AƨA?}A��A5?A�A�A��A�A��AM�A��A
��A
�A	��A	C�AZAƨAC�A9XAM�A?}AĜAM�A�A b@��@��@�X@�9X@�|�@��y@�M�@�O�@�t�@��y@�$�@�&�@��@�+@�$�@�?}@�9X@�"�@��@�%@�u@�P@�v�@�hs@�@��@�j@�t�@�V@�@�G�@�?}@���@�Z@�1@�dZ@�hs@֏\@��T@���@ӍP@ҟ�@�M�@��#@�?}@��`@�j@�"�@��@͙�@̣�@��@�-@�O�@ȋD@ǝ�@�C�@�{@�r�@�@�V@� �@�=q@�&�@�I�@���@�\)@��@���@�@�/@���@�j@�  @��F@���@�^5@�G�@���@��@��D@�bN@�1@��m@��P@���@���@��R@�^5@��@���@�x�@�O�@��@���@� �@�ƨ@�C�@�
=@��H@�^5@��@�@�`B@��@���@��@��F@�t�@�o@��\@�ff@�$�@��@��#@���@�x�@�7L@�Z@�1@�1'@���@�S�@��y@���@�v�@�M�@���@��7@�hs@�X@�&�@�V@��@��D@�r�@��
@�t�@���@��@��w@���@�K�@�@��R@�~�@��^@�&�@��`@���@�(�@���@��@�S�@�^5@�=q@�@�$�@��@�J@���@��@�/@�bN@��@��;@��w@��w@�ƨ@��@�|�@�S�@�C�@�ȴ@���@�~�@�ff@�{@��@�O�@�`B@�`B@��@���@�(�@�ƨ@�;d@�C�@���@�
=@��\@�5?@�-@��@�J@���@�%@��D@�I�@�A�@��@��F@��@��;@�dZ@���@��+@�5?@��-@�O�@��@���@��9@���@��@�Z@� �@�  @��F@��P@�|�@�33@��@���@�v�@�M�@�J@��#@�hs@�&�@�V@��@��@�Ĝ@��@�9X@�1'@�b@�ƨ@���@�l�@�
=@��@���@�~�@�=q@�@��@��@�$�@��7@�`B@�G�@�?}@�?}@�7L@�7L@�&�@��@��/@�A�@�9X@�(�@�b@���@���@���@��@�+@��R@�^5@��T@�x�@��@���@�(�@��m@�|�@�\)@�K�@�o@��@��@���@��+@�ff@�M�@�5?@��7@��@��@���@���@�Ĝ@�bN@�Z@�Q�@�9X@���@��9@�(�@K�@~��@}��@}��@}��@}`B@}/@|�D@|9X@|1@{ƨ@{�F@{33@z��@z��@z��@z��@y��@x1'@w��@u��@t��@tj@tZ@s��@s@r��@r��@r��@r��@s@r^5@q�^@q�#@q��@q��@q&�@p��@p�9@p�@p �@nV@m�h@k�F@k�@kS�@j�\@j~�@j�!@kt�@kS�@kS�@j��@jM�@i7L@h��@h�9@hb@g��@g�w@g��@g
=@fv�@f5?@fE�@f{@e�h@e/@d��@d��@dI�@d1@cƨ@cdZ@b�!@a�@a�^@a��@aX@a%@`��@`�9@`�9@`r�@` �@_��@_�@^ȴ@^�+@^ff@^V@^E�@^E�@]�@]p�@]`B@]O�@\��@\�D@\Z@[ƨ@Z�@Z�H@Z��@Z�\@Zn�@Zn�@Zn�@Zn�@Zn�@Z^5@Z^5@Z-@Y�@Y��@Yhs@Y&�@X�u@Xr�@XbN@XQ�@X1'@W�@W;d@V�y@V��@Vff@V{@U��@U`B@UO�@T��@T�/@T�D@S�
@SS�@S"�@So@R�@R��@R^5@Q�^@Q7L@Qx�@QG�@P�u@O�w@OK�@O+@N�y@Nȴ@N��@N�+@NV@NE�@NE�@N$�@M�T@Mp�@L��@L�/@L��@L�@L(�@K�m@K��@KdZ@K33@K"�@J�@J�H@J��@J�!@JJ@I��@Ihs@H��@HA�@Hb@G�P@G;d@F�@F��@FE�@F{@F@F@E�-@Ep�@E/@D��@D�D@D1@Cƨ@C�@CdZ@B��@B^5@B=q@A�@A�7@A�@@�`@@�u@@Q�@@A�@@1'@?�@?�w@?�@?��@?�P@?|�@?+@>�y@>��@=�T@=/@<�/@<z�@<z�@<j@<Z@<1@;��@:��@:�!@:~�@:^5@9�@9�^@9x�@9X@9%@8��@8 �@7��@7|�@7K�@7+@7
=@6��@6E�@65?@5�@5@5�@5O�@5/@4�@4�j@4��@49X@4(�@3��@3ƨ@3�F@3��@3�@3S�@3C�@3o@2�H@2��@2n�@2=q@2J@1��@1��@1&�@0�`@0�9@0r�@0bN@0bN@0Q�@0A�@/�@/��@/�P@/K�@/+@/
=@.�R@.�+@.@-��@-`B@-O�@-�@,�@,�/@,��@,�j@,��@,��@,j@,I�@,(�@+��@+��@+S�@+@*�!@*�\@*-@)�^@)hs@(Ĝ@(��@(�@(bN@'�;@'|�@'\)@'l�@'K�@'
=@&��@&�+@&5?@&{@&@%�@%�T@%��@%�@%/@$��@$��@$��@$z�@$Z@$Z@$9X@#�m@#��@#��@#t�@#S�@#"�@"��@"�\@"�@!�^@!�^@!��@!X@ �`@ r�@  �@�@��@��@l�@ȴ@��@��@��@�+@E�@�@��@�h@�@�@O�@��@�j@�j@�@j@�m@ƨ@��@C�@��@��@~�@M�@�@��@�7@�@Ĝ@�u@A�@  @�w@�P@\)@K�@�@ȴ@�R@�+@V@5?@@�T@�-@�@V@��@�D@z�@�@�F@��@dZ@S�@C�@"�@�@�!@n�@M�@=q@��@��@x�@G�@%@��@�u@r�@Q�@Q�@1'@b@  @�;@��@\)@;d@�@��@�y@ȴ@��@�+@ff@5?@�@@@�-@�-@�@`B@O�@V@�j@��@�D@Z@I�@I�@(�@�@��@�
@�
@ƨ@��@dZ@S�@C�@o@
��@
�\@
~�@
n�@
=q@
-@
�@
J@	��@	��@	�@	�#@	�^@	��@	�7@	x�@	X@	&�@	�@�`@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
bB
hB
.B
#�B
 �B
/B
,B
0!B
F�B
I�B
L�B
W
B
e`B
�'B
��B
��B�BjB��B�FBǮB�#B��B��B�BVBPB�B0!B5?B<jBC�BE�BffBp�Bp�Bv�Bw�Bn�Bu�By�By�B�%B�B�%Bx�BjBm�BXBC�B=qB5?B�BPBVB\B�B�BhBB�B��BǮB��B�wB��B��B��B�oB� Bs�B[#BZBB�B>wB+B2-B�BB
��B
��B
ÖB
�B
�+B
�JB
�1B
o�B
bNB
S�B
S�B
;dB
�B
�B
�B
�B
�B

=B	��B	��B	�B	�ZB	�HB	�#B	��B	ÖB	��B	�RB	�B	�B	��B	��B	�{B	{�B	gmB	� B	o�B	C�B	C�B	?}B	2-B	!�B	.B	6FB	,B	oB	\B	VB	B�mB�ZB�mB�`B�HBŢBƨB�3B��B��B��B�B��B��B��B��B��B��B��B�{B�+B�bB�DB�hB�B�+B�=B}�Bt�Bp�Bq�B~�Bt�Bo�BhsBr�Bw�Bw�Bw�Bu�Bp�Bt�Bo�BhsBp�Bn�Bl�Be`BbNBS�BH�B`BBaHBXBH�BK�BM�BbNBcTBcTBbNB[#BffB`BBcTB^5BcTBbNBZBN�BZBdZBcTBXB[#BXBR�B`BBcTBgmBgmBffBdZB_;Bk�Bk�BiyBjBjBk�Bk�Bl�Bk�Bl�Bk�Bn�BiyBhsBgmBdZBcTBiyBhsBjBq�Br�Bu�Br�Bn�Bn�BhsB^5B^5Bs�Bo�Bs�By�B�B�B�B�B�B|�B� B�=B�%B�B�PB�PB�bB�bB�uB�VB�JB��B��B��B��B�B�-B�LB�qB�wB�qB�}BŢBɺB��B��B��B��B�
B��B�BB�HB�TB�ZB�`B�sB�mB�sB�B��B�B��B��B��B��B��B��B	B	+B	JB	oB	uB	uB	�B	�B	�B	�B	�B	�B	$�B	&�B	'�B	'�B	-B	.B	0!B	33B	2-B	49B	6FB	8RB	B�B	H�B	I�B	H�B	O�B	R�B	T�B	T�B	T�B	XB	_;B	aHB	`BB	aHB	bNB	ffB	m�B	jB	p�B	t�B	� B	~�B	� B	�B	�B	�B	�B	�B	�B	�7B	�=B	�=B	�PB	�oB	�oB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�FB	�^B	�dB	�^B	�jB	�dB	�qB	��B	ÖB	ĜB	ǮB	ƨB	ȴB	��B	��B	��B	ɺB	ǮB	ȴB	ɺB	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�5B	�5B	�;B	�HB	�BB	�HB	�NB	�HB	�NB	�TB	�`B	�`B	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B

=B

=B
JB
JB
DB

=B
PB
PB
JB
	7B
	7B
JB
DB
hB
VB
\B
oB
oB
uB
�B
�B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
!�B
 �B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
&�B
&�B
(�B
'�B
&�B
$�B
&�B
(�B
&�B
'�B
(�B
'�B
&�B
&�B
'�B
(�B
(�B
'�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
-B
-B
-B
-B
.B
/B
/B
.B
.B
.B
.B
/B
/B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
2-B
0!B
1'B
0!B
/B
.B
2-B
2-B
2-B
49B
49B
49B
49B
49B
49B
49B
33B
2-B
33B
33B
33B
33B
5?B
6FB
6FB
6FB
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
8RB
8RB
8RB
7LB
8RB
;dB
;dB
:^B
:^B
9XB
9XB
9XB
<jB
:^B
9XB
8RB
9XB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
<jB
;dB
=qB
>wB
?}B
?}B
>wB
?}B
@�B
A�B
A�B
B�B
A�B
B�B
A�B
A�B
?}B
@�B
A�B
?}B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
E�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
F�B
H�B
I�B
K�B
K�B
K�B
J�B
H�B
H�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
K�B
M�B
M�B
N�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
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
W
B
XB
W
B
XB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
YB
ZB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
\)B
\)B
\)B
\)B
\)B
]/B
^5B
_;B
^5B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
aHB
aHB
aHB
aHB
bNB
cTB
cTB
aHB
aHB
bNB
cTB
cTB
dZB
e`B
dZB
cTB
e`B
ffB
ffB
ffB
e`B
e`B
e`B
gmB
gmB
gmB
ffB
ffB
gmB
hsB
hsB
gmB
ffB
hsB
hsB
hsB
gmB
iyB
iyB
iyB
iyB
hsB
iyB
hsB
iyB
jB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
o�B
q�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
v�B
v�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
y�B
x�B
x�B
x�B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
z�B
{�B
{�B
|�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
B
�B
+B
@B
{B
.�B
%�B
"�B
0!B
-B
1'B
F�B
JrB
NB
X�B
hsB
��B
��B
�B!�Bm�B�kB��B�#B�~B�B��B�TB�BBB!�B1�B7LB>�BFtBIBh$Bq�BrBw�Bx�BpUBv�B{0B{dB��B�GB�B{JBmCBo�B[�BG�B@�B8�B �B�BHB�B�B�B BgB��B�SB�#B�jB�OB��B�sB�B�{B��BvB^OB\xBF%B@�B-wB3�B \BzB
ѷB
��B
�SB
��B
��B
��B
�	B
rB
d�B
VmB
V�B
>�B
)B
�B
$B
sB
9B
xB	��B	�LB	��B	�B	��B	��B	��B	ňB	��B	�B	��B	��B	�B	�B	�SB	~�B	j�B	��B	qvB	HB	F?B	A�B	5ZB	$�B	/5B	6�B	,�B	gB	hB	�B	MB�QB�RB��B�B��B�B��B�zB�*B��B�eB�}B��B�B��B�@B�sB��B��B�9B�7B��B��B�:B�%B�fB�)B�Bv�Bs3BsB}Bv`BqABjeBs�Bx�BxlBxRBvzBq�Bu?Bp�Bi�Bq'BoOBm)BffBc�BVBK�B`�Ba�BYeBJ�BN"BO�Bb�BdBc�Bc:B\]Bf�BabBd@B_pBd@BcTB[�BQNB[qBeBd@BZB\�BY�BT�Ba-Bd&Bg�Bg�BgBe,B`\Bk�Bl"BjeBkQBk6BlWBl=Bm]BlWBmwBlWBo BjKBi_BhsBe�BdtBjeBi_BkQBr-BsBu�Br�Bo5Bo BiDB_�B`'BtBp�Bt�Bz�B�UB�uB��B�{B��B~B��B��B��B�MB�B�B�B�B��B�\B��B��B�jB�1B��B��B��B��B��B��B��B�B�B�#B�)B�6B�4B�TB��B��B�vB�B�B�B�B�B��B��B�B��B�B�B�>B�.B�HB�HB�]B	uB	zB	�B	�B	�B	�B	�B	�B	�B	B	 'B	;B	%,B	'8B	(XB	(XB	-CB	.IB	0UB	3MB	2aB	4nB	6�B	8�B	B�B	H�B	J	B	I7B	P.B	S&B	U2B	U2B	UMB	X_B	_VB	a|B	`vB	a|B	b�B	f�B	m�B	kB	p�B	t�B	�B	.B	�4B	�;B	�[B	�[B	�aB	��B	�{B	�lB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	� B	�-B	�;B	��B	�B	�B	��B	��B	�
B	�$B	�$B	�B	�DB	�CB	�/B	�5B	�iB	��B	�zB	�^B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	�B	��B	��B	��B	��B	�#B	�KB	�B	��B	��B	�<B	��B	��B	�B	�TB	�FB	�[B	�MB	�[B	�MB	�EB	�KB	�WB	�CB	�OB	�jB	�pB	�bB	�vB	�|B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�DB	�B	�B	��B	��B	��B
  B	�.B
 B	�.B	�6B
B
'B
-B
'B
AB
GB
GB
aB
[B
aB
uB
aB
aB
uB
[B
MB
mB
_B

XB

rB
JB
dB
xB

�B
jB
jB
~B
	�B
	�B
~B
xB
4B
�B
�B
�B
oB
uB
gB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
!�B
 �B
"�B
"�B
!�B
 'B
CB
B
1B
�B
�B
B
"�B
$�B
&�B
'B
)B
($B
'B
%FB
'B
)B
'B
($B
)B
(
B
'B
'B
(
B
(�B
)B
($B
)B
*B
)*B
)B
)B
)B
)*B
)DB
)DB
-)B
-CB
-)B
-CB
./B
/5B
/5B
./B
./B
.IB
.IB
/OB
/5B
1AB
1'B
1AB
2-B
1vB
1[B
2-B
2GB
0UB
1AB
0;B
/iB
.cB
2-B
2GB
2GB
49B
49B
49B
49B
49B
49B
49B
3MB
2GB
3hB
3MB
3MB
3hB
5ZB
6FB
6`B
6zB
4nB
5tB
6`B
6zB
7fB
7fB
7�B
8lB
9�B
8lB
8lB
8�B
7�B
8�B
;B
;dB
:xB
:xB
9�B
9�B
9�B
<jB
:�B
9�B
8�B
9�B
<�B
<�B
<�B
<�B
<�B
<�B
=qB
=�B
<�B
<�B
;�B
=�B
>�B
?�B
?�B
>�B
?�B
@�B
A�B
A�B
B�B
A�B
B�B
A�B
A�B
?�B
@�B
A�B
?�B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
E�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
GB
F�B
H�B
I�B
K�B
K�B
K�B
J�B
H�B
H�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
K�B
M�B
M�B
N�B
OB
PB
OB
N�B
O�B
O�B
PB
PB
Q B
Q B
Q B
RB
RB
Q B
Q�B
RB
RB
R�B
S�B
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
UB
U2B
VB
W$B
W$B
XB
XB
XB
W$B
W$B
W$B
W$B
W$B
X+B
XEB
W$B
X+B
W$B
X+B
Y1B
ZB
Y1B
Z7B
ZB
ZB
ZB
Z7B
[#B
Z7B
Z7B
Z7B
Z7B
YeB
ZQB
Z7B
Z7B
[=B
ZkB
ZQB
ZQB
ZQB
\CB
\CB
\)B
\]B
\CB
]IB
^5B
_pB
^OB
]IB
^OB
^OB
_VB
_;B
_;B
_;B
_VB
_VB
_VB
_VB
`\B
`\B
`\B
abB
aHB
abB
abB
abB
bNB
bhB
bhB
a|B
a|B
a|B
a|B
b�B
cnB
cnB
a|B
a|B
b�B
cnB
cnB
dtB
e`B
d�B
c�B
e`B
ffB
fLB
f�B
ezB
ezB
e�B
g�B
gmB
gmB
f�B
f�B
g�B
h�B
h�B
g�B
f�B
h�B
h�B
h�B
g�B
i�B
i�B
i�B
i�B
h�B
i�B
h�B
i�B
j�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
o�B
q�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
w�B
xB
v�B
v�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y	B
x�B
x�B
y�B
y�B
y	B
x�B
y	B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
z�B
z�B
{�B
|B
|B
z�B
{�B
|B
}B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806260034512018062600345120180626003451201806260200302018062602003020180626020030201806270027252018062700272520180627002725  JA  ARFMdecpA19c                                                                20180622093526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180622003542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180622003546  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180622003546  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180622003547  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180622003547  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180622003547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180622003547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180622003547  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180622003547                      G�O�G�O�G�O�                JA  ARUP                                                                        20180622005722                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180622153657  CV  JULD            G�O�G�O�F�a�                JM  ARCAJMQC2.0                                                                 20180625153451  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180625153451  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180625170030  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180626152725  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                