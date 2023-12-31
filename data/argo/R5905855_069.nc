CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:22:52Z creation;2022-06-04T19:22:52Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604192252  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�R�/��k1   @�R����@,Y�+�c���$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B33B   B(  B0  B8��B@  BH  BP��BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8Q�@~�R@�(�@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B�B�B�B'�B/�B8�RB?�BG�BP�RBW�B_�Bg�Bo�Bw�B�B�B���B���B�\)B���B��]B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�\)B���B�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C{C�GC��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C@{CB{CC�GCE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck�GCm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�DD~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DCxRDC��DD~�DD��DE~�DE��DF�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ȀA��)A���A��<A��<A�ȴA�ɺA��#A�ʌA���A��pA��A��aA�҉A���A��[A�ϫA�ĜAͺ^A���A�K)Aƥ�A�g8A�XA�uA�B�A�@OA�l"A���A�ںA�m�A�4�A��A��8A�XA��A�l�A���A�  A���A���A�.IA�IA�V�A��\A���A�@�A��&A��A���A�� A��A��sA��QA���A��}A��VA��A��A�
�A�MA��A�D�A��A�!�A�`A�iyA�/�A��)A�~A� �ADgA{��Ax�Au�zAp�TAm��Al	lAj*0Aie�Af��AbX�A_�A[�4AXZ�AW%FAU~AR�AL�VAI iAGtTAB�A>F�A;�qA8@OA533A3��A2�A1��A1oA0��A0�RA.�$A,˒A,1A+�A(b�A%��A%l�A&ffA'n�A-oiA.�eA-�A+xA*h�A&�"A%�A#{JA"�wA!��A �+A�A(A҉APHAK^Ay>Ao A�wA#�A,=AA�RA�6AxAJ#A��A��AԕA($A�mA�AC-A@OA-wAR�A��AP�A
�AqA
��A
@A	  A	��A
G�A
��A  A
�4A	��AѷA�sA?}A"�As�ACA��AD�A��Au�A�AT�ASA��A-A�$A��A
�	A
5?A
$tA	x�A	�A�A(�A�'A�CA�A'RA�A�YAY�A0UA��A��A��A�4Ah�A!-A��A�AqvAC-A {JA >BA �$A tTA @���@���@��:@��^@��"@�7�@�S�@��@�;d@�q@���@�a@�/@��@�P@��U@�-@���@���@�$t@�ȴ@�@���@��,@�@�@�˒@��@귀@�e@��a@��@�7@���@�?}@��@�8�@�͟@�J@���@㞄@�k�@�$�@��@�u@߄M@��}@��@ܵ�@��@��3@�+�@ڜx@�=q@�_p@�Ɇ@ر�@؋D@��+@��@�^5@�I�@��@�"�@Ԙ_@�j@ө*@�F@��X@��3@є�@�.I@Ў�@�:*@�'R@ύP@��|@���@Σ@�?�@��@͡�@͠�@͔�@�k�@��2@̻�@�j@ʨ�@�M@��@��)@��T@ɿH@�p�@ȯO@�!@Ǐ�@�4@���@ƣ@�M@��@ŷ�@�w2@�:�@ĸR@Æ�@���@F@�M@�,=@�#:@���@��q@��h@�J#@��P@��1@�+k@��f@�@�V�@���@�Ĝ@�a|@�_@��@��f@��@��	@���@�e@���@�S@�z@�J@��S@�g�@�7L@�%F@��@���@�n�@��@��g@���@�Mj@���@��u@��@�f�@��y@���@��@�?�@��0@��8@�s�@�Z�@�R�@�A�@��@�x�@��@�oi@��@��C@�J�@��@�:*@��w@�a�@��@�.�@���@��@���@�y�@�c�@�F@��@�֡@���@���@��u@�g8@��=@�K�@�K�@�A @�@O@���@���@�z�@�e�@�l�@�I�@�5?@�7@� �@��]@���@��K@��@�.I@���@���@��o@�3�@���@��$@�\)@��@�1'@���@��P@�1�@�U2@��D@���@��@�	l@�ߤ@��b@��@�>B@��@��@��j@��[@�+�@���@��r@��@��h@�1�@��X@���@�~�@�j@�D�@�)�@�O@��@��}@�@O@���@���@�v�@�M�@��@���@�U�@��@���@�C�@���@��@��p@��\@�6@���@��4@���@�-�@�H�@�/�@���@���@�Z�@���@�hs@�*0@�(@���@�Ɇ@��r@�8�@��h@�(�@��@���@�tT@�R�@��@��@���@�o @��f@��$@�r�@�9X@��;@�ƨ@��*@�l�@�V@�͟@�e�@�4@��@���@�� @��f@�^�@�$t@���@��r@�9X@���@���@�K�@�
=@���@���@�kQ@�PH@�/�@��@��@��H@�L�@��@�͟@�W�@��@��t@�^�@�&�@��@���@�ff@��@�_@��@��@�
@��@�f@�@~a|@}�-@}%F@|�@{��@{�q@{��@{{J@z��@z=q@z�@z	@y�@y�C@x��@x1'@x@w�&@w�a@wRT@v͟@vV@u��@u�S@u\�@u8�@t�U@tC-@t'R@s�@siD@r��@r_�@r�@q�Z@qԕ@q@q�X@p��@pq@p4n@pG@o��@o�@n��@n��@n$�@m��@m��@m%@lz�@lj@l_@l>B@k�+@k� @k��@k��@kt�@kS�@k�@j�h@j5?@j�@i�@h�@h%�@g�w@gg�@g&@fYK@fM�@f	@eT�@d�@d~(@d]d@d@cv`@b��@bc @a�@`�v@`w�@`%�@_��@_E9@_"�@^͟@]��@]�@\�@\w�@[�
@[j�@Z��@Y\�@YF@Y@@X��@X�@X��@Xu�@X%�@W�K@WS@V�6@V� @V�A@V)�@V	@U�@U|@T�O@T2�@S��@S�@R��@R0U@Q�@Q�@Q8�@Q�@Py>@O�@O"�@N��@N�'@NOv@M�#@M�@Mc�@L�@Lb@K��@K,�@J�B@Jh
@J;�@I�.@I�-@Ic�@IN<@I!�@H�@H�[@H�Y@H@G��@G�@F��@E�@E��@E�M@E&�@D�z@C�+@Cb�@C6z@C�@C�@B��@B��@B��@BTa@B	@A��@A��@AS&@A!�@@j@?��@?$t@>��@>GE@=�z@=X@=*0@=+@<��@;�@;��@;j�@;Z�@;9�@;Y@:�!@:Ov@9�D@9�#@9��@9ϫ@9�3@9��@9��@9�@8��@8��@8�@8?�@7�W@7��@7�f@7"�@6�2@6��@6($@5��@5(�@4��@4��@4l"@4c�@41'@3�]@3��@3�f@3s@3RT@3J#@3.I@3�@2�B@2��@2
�@1��@1�"@1Y�@0�@0��@0:�@0�@/=@.�6@.R�@-�.@-�-@-f�@,�v@,9X@,�@+�@+��@+n/@+iD@+iD@+a@+6z@+�@*��@)�@)|@)o @)�@(�@(��@( �@'��@'C�@&��@&�b@&W�@&e@%O�@%�@$�z@$oi@$�@#>�@"�8@"�s@"��@"_�@"�@!�@!�S@!&�@ �[@ ��@ h�@ $@�K@��@\)@/�@��@�,@��@5?@��@��@��@c�@?}@��@�j@�@�D@]d@-�@��@�@�y@�]@�s@ȴ@��@��@J�@@��@�'@f�@?}@2a@�@Ɇ@��@��@��@��@m�@6@�@��@��@+@��@�r@p;@W�@6�@�)@ϫ@�H@�^@�@��@��@�@��@��@��@=�@�@�E@��@��@�.@u�@Z@M@ �@�@�W@˒@��@\)@$t@S@ i@ i@@@@ i@�"@��@ȴ@� @��@h
@E�@e@��@�@ϫ@�d@�z@�3@�@�3@��@m]@G�@8�@2a@@�@�v@y>@bN@*�@��@�@�:@x@a@"�@��@p;@Ta@+k@	@��@��@k�@A @	l@�?@Xy@7�@~@�@�K@�@�$@�:@��@��@{J@{J@x@g�@Z�@/�@
�]@
�@
��@
u%@
s�@
n�@
J@	�)@	��@	ϫ@	�@	�@	hs@	`B@	<6@	�@��@��@D�@�@1@1@��@��@�q@�$@Z�@J#@>�@&@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ȀA��)A���A��<A��<A�ȴA�ɺA��#A�ʌA���A��pA��A��aA�҉A���A��[A�ϫA�ĜAͺ^A���A�K)Aƥ�A�g8A�XA�uA�B�A�@OA�l"A���A�ںA�m�A�4�A��A��8A�XA��A�l�A���A�  A���A���A�.IA�IA�V�A��\A���A�@�A��&A��A���A�� A��A��sA��QA���A��}A��VA��A��A�
�A�MA��A�D�A��A�!�A�`A�iyA�/�A��)A�~A� �ADgA{��Ax�Au�zAp�TAm��Al	lAj*0Aie�Af��AbX�A_�A[�4AXZ�AW%FAU~AR�AL�VAI iAGtTAB�A>F�A;�qA8@OA533A3��A2�A1��A1oA0��A0�RA.�$A,˒A,1A+�A(b�A%��A%l�A&ffA'n�A-oiA.�eA-�A+xA*h�A&�"A%�A#{JA"�wA!��A �+A�A(A҉APHAK^Ay>Ao A�wA#�A,=AA�RA�6AxAJ#A��A��AԕA($A�mA�AC-A@OA-wAR�A��AP�A
�AqA
��A
@A	  A	��A
G�A
��A  A
�4A	��AѷA�sA?}A"�As�ACA��AD�A��Au�A�AT�ASA��A-A�$A��A
�	A
5?A
$tA	x�A	�A�A(�A�'A�CA�A'RA�A�YAY�A0UA��A��A��A�4Ah�A!-A��A�AqvAC-A {JA >BA �$A tTA @���@���@��:@��^@��"@�7�@�S�@��@�;d@�q@���@�a@�/@��@�P@��U@�-@���@���@�$t@�ȴ@�@���@��,@�@�@�˒@��@귀@�e@��a@��@�7@���@�?}@��@�8�@�͟@�J@���@㞄@�k�@�$�@��@�u@߄M@��}@��@ܵ�@��@��3@�+�@ڜx@�=q@�_p@�Ɇ@ر�@؋D@��+@��@�^5@�I�@��@�"�@Ԙ_@�j@ө*@�F@��X@��3@є�@�.I@Ў�@�:*@�'R@ύP@��|@���@Σ@�?�@��@͡�@͠�@͔�@�k�@��2@̻�@�j@ʨ�@�M@��@��)@��T@ɿH@�p�@ȯO@�!@Ǐ�@�4@���@ƣ@�M@��@ŷ�@�w2@�:�@ĸR@Æ�@���@F@�M@�,=@�#:@���@��q@��h@�J#@��P@��1@�+k@��f@�@�V�@���@�Ĝ@�a|@�_@��@��f@��@��	@���@�e@���@�S@�z@�J@��S@�g�@�7L@�%F@��@���@�n�@��@��g@���@�Mj@���@��u@��@�f�@��y@���@��@�?�@��0@��8@�s�@�Z�@�R�@�A�@��@�x�@��@�oi@��@��C@�J�@��@�:*@��w@�a�@��@�.�@���@��@���@�y�@�c�@�F@��@�֡@���@���@��u@�g8@��=@�K�@�K�@�A @�@O@���@���@�z�@�e�@�l�@�I�@�5?@�7@� �@��]@���@��K@��@�.I@���@���@��o@�3�@���@��$@�\)@��@�1'@���@��P@�1�@�U2@��D@���@��@�	l@�ߤ@��b@��@�>B@��@��@��j@��[@�+�@���@��r@��@��h@�1�@��X@���@�~�@�j@�D�@�)�@�O@��@��}@�@O@���@���@�v�@�M�@��@���@�U�@��@���@�C�@���@��@��p@��\@�6@���@��4@���@�-�@�H�@�/�@���@���@�Z�@���@�hs@�*0@�(@���@�Ɇ@��r@�8�@��h@�(�@��@���@�tT@�R�@��@��@���@�o @��f@��$@�r�@�9X@��;@�ƨ@��*@�l�@�V@�͟@�e�@�4@��@���@�� @��f@�^�@�$t@���@��r@�9X@���@���@�K�@�
=@���@���@�kQ@�PH@�/�@��@��@��H@�L�@��@�͟@�W�@��@��t@�^�@�&�@��@���@�ff@��@�_@��@��@�
@��@�f@�@~a|@}�-@}%F@|�@{��@{�q@{��@{{J@z��@z=q@z�@z	@y�@y�C@x��@x1'@x@w�&@w�a@wRT@v͟@vV@u��@u�S@u\�@u8�@t�U@tC-@t'R@s�@siD@r��@r_�@r�@q�Z@qԕ@q@q�X@p��@pq@p4n@pG@o��@o�@n��@n��@n$�@m��@m��@m%@lz�@lj@l_@l>B@k�+@k� @k��@k��@kt�@kS�@k�@j�h@j5?@j�@i�@h�@h%�@g�w@gg�@g&@fYK@fM�@f	@eT�@d�@d~(@d]d@d@cv`@b��@bc @a�@`�v@`w�@`%�@_��@_E9@_"�@^͟@]��@]�@\�@\w�@[�
@[j�@Z��@Y\�@YF@Y@@X��@X�@X��@Xu�@X%�@W�K@WS@V�6@V� @V�A@V)�@V	@U�@U|@T�O@T2�@S��@S�@R��@R0U@Q�@Q�@Q8�@Q�@Py>@O�@O"�@N��@N�'@NOv@M�#@M�@Mc�@L�@Lb@K��@K,�@J�B@Jh
@J;�@I�.@I�-@Ic�@IN<@I!�@H�@H�[@H�Y@H@G��@G�@F��@E�@E��@E�M@E&�@D�z@C�+@Cb�@C6z@C�@C�@B��@B��@B��@BTa@B	@A��@A��@AS&@A!�@@j@?��@?$t@>��@>GE@=�z@=X@=*0@=+@<��@;�@;��@;j�@;Z�@;9�@;Y@:�!@:Ov@9�D@9�#@9��@9ϫ@9�3@9��@9��@9�@8��@8��@8�@8?�@7�W@7��@7�f@7"�@6�2@6��@6($@5��@5(�@4��@4��@4l"@4c�@41'@3�]@3��@3�f@3s@3RT@3J#@3.I@3�@2�B@2��@2
�@1��@1�"@1Y�@0�@0��@0:�@0�@/=@.�6@.R�@-�.@-�-@-f�@,�v@,9X@,�@+�@+��@+n/@+iD@+iD@+a@+6z@+�@*��@)�@)|@)o @)�@(�@(��@( �@'��@'C�@&��@&�b@&W�@&e@%O�@%�@$�z@$oi@$�@#>�@"�8@"�s@"��@"_�@"�@!�@!�S@!&�@ �[@ ��@ h�@ $@�K@��@\)@/�@��@�,@��@5?@��@��@��@c�@?}@��@�j@�@�D@]d@-�@��@�@�y@�]@�s@ȴ@��@��@J�@@��@�'@f�@?}@2a@�@Ɇ@��@��@��@��@m�@6@�@��@��@+@��@�r@p;@W�@6�@�)@ϫ@�H@�^@�@��@��@�@��@��@��@=�@�@�E@��@��@�.@u�@Z@M@ �@�@�W@˒@��@\)@$t@S@ i@ i@@@@ i@�"@��@ȴ@� @��@h
@E�@e@��@�@ϫ@�d@�z@�3@�@�3@��@m]@G�@8�@2a@@�@�v@y>@bN@*�@��@�@�:@x@a@"�@��@p;@Ta@+k@	@��@��@k�@A @	l@�?@Xy@7�@~@�@�K@�@�$@�:@��@��@{J@{J@x@g�@Z�@/�@
�]@
�@
��@
u%@
s�@
n�@
J@	�)@	��@	ϫ@	�@	�@	hs@	`B@	<6@	�@��@��@D�@�@1@1@��@��@�q@�$@Z�@J#@>�@&@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B��B�B�TB��B�B�B�B�B�B��B��B�2B��B��B�%B��B͟B�B
��B'B+�B+QB+QB)�B$tB 'BxB�B9B/B-]BF�BP�BhsB��B�B��B�YB��B��B��B�HB��BˬB�;B�B)B�zB�vB��B�B�B�BgB[	Bm�BoiBG�B%�B!�B4B"�B�B
��B
��B
��B
x8B
V�B
HfB
FYB
,�B
�B
  B	�8B	�%B	��B	��B	��B	�zB	x�B	\�B	N�B	7�B	$B	�B	�B�}B�XB�SB�XB�(B��B��B�tB�,B��B��B��B��B�B�qB��B�aB�,B�B��B�TB�B�fB�B	`�B	�JB	�fB	��B	��B	q�B	g�B	_pB	\xB	a�B	eFB	d&B	e,B	ffB	gB	n/B	s�B	v�B	u�B	o5B	e�B	LB	5tB	%,B	(�B	!-B	�B	B	�B	kB	�B	�B	EB	(�B	3hB	<�B	C�B	F?B	D�B	E�B	A�B	@B	C{B	P�B	f�B	n/B	utB	raB	qvB	�NB	��B	B	�B	�B	��B	��B	�rB	�VB
�B
 B	��B	�B
  B
 iB	��B
B
'B
 iB	��B
B
�B
�B

�B
�B
B
�B
0B
B
B
�B
B
jB
�B
�B
�B
�B
�B
B
VB
�B	�<B	��B	�DB
 �B
�B
�B
aB	�cB	��B	�lB	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�"B	��B	�QB	��B	�wB	�wB	��B	�QB	��B	�|B	�B	�B	��B	�nB	��B	��B	��B	��B	��B	�!B	��B	��B	��B	�B	�|B	�hB	�B	�vB	�iB	�qB	�QB	�B	�0B	��B	�B	�B	�B	�B	��B	�B	� B	�B	�}B	��B	�)B	�"B	�B	�)B	�IB	�B	�)B	�=B	�"B	�B	�qB	�B	�WB	�B	�6B	�B	�QB	�B	�B	�6B	�6B	�kB	�B	�B	�wB	��B	��B	�)B	�]B	�]B	�CB	��B	�B	�vB	�-B	�B	�MB	�MB	�hB	��B	�B	�B	�3B	��B	�B	�hB	�B	�MB	�B	�TB	�nB	�?B	��B	��B	��B	��B	��B	�B	�B	�RB	�RB	�	B	��B	�B	��B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�B	��B	��B	�BB	�(B	�(B	�wB	��B	�B	�cB	�cB
 B
 iB
 �B
 �B
oB
-B
-B
B
�B
GB
�B
�B
�B
�B
�B
?B
�B
�B
B
�B
KB
1B
KB
	B
�B
�B
	B
	�B
	B
	lB
	�B

rB
�B
�B
�B
�B
dB
dB
0B
JB
B
<B
VB
B
�B
[B
�B
B
�B
MB
�B
mB
�B
�B
�B
�B
�B

B
YB
YB
$B

B
�B
�B
B
�B
MB
�B
�B
�B
2B
B
SB
�B
�B
�B
�B
mB
SB
�B
SB
B
9B
B
�B
$B
�B
�B
1B
�B
QB
kB
�B
QB
�B
�B
kB
kB
7B
�B
�B
	B
�B
�B
	B
�B
�B
B
xB
�B
�B
�B
B
�B
OB
B
jB
�B
�B
�B
5B
�B
~B
�B
B
!B
!B
!B
;B
pB
�B
�B
 B
�B
 �B
 �B
 �B
!|B
!�B
"4B
"hB
"hB
"NB
"hB
# B
# B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&B
%�B
&B
&2B
&�B
&�B
'B
'�B
'�B
'�B
(
B
(�B
)*B
)*B
)DB
)B
)*B
)*B
)DB
)*B
)�B
*0B
)�B
+6B
+6B
,"B
,qB
,�B
-]B
-wB
-]B
-�B
.B
-�B
-�B
-�B
-�B
.B
./B
./B
-�B
-]B
,�B
,�B
,�B
-CB
-]B
-)B
-�B
/�B
0B
0!B
0B
0!B
0B
1'B
1B
1'B
1'B
1[B
2-B
3MB
3hB
3�B
3�B
4TB
4�B
5?B
5tB
5ZB
5tB
5�B
5�B
6+B
6FB
6`B
6`B
6`B
6FB
6�B
6�B
7B
7B
7�B
7�B
7�B
7�B
8B
8RB
8lB
8�B
9	B
9	B
9	B
9	B
9>B
9>B
9rB
9XB
9�B
9�B
9�B
9�B
:^B
:^B
:�B
;�B
;�B
<B
<�B
<�B
=�B
=qB
=�B
>BB
>�B
>�B
>�B
>�B
>�B
?.B
>�B
?HB
@iB
@iB
@�B
@�B
AB
@�B
AB
A�B
A�B
A�B
BAB
B�B
B�B
C�B
D�B
D�B
EB
EB
EmB
E�B
F%B
FtB
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IRB
J	B
JrB
J�B
K�B
K�B
L0B
L0B
L~B
L�B
LdB
MB
MjB
M�B
M�B
M�B
N<B
N�B
N�B
N�B
N�B
O�B
O�B
P.B
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T{B
T�B
UB
U2B
U2B
U2B
U�B
U�B
VB
V9B
V9B
V�B
VmB
V�B
W
B
W�B
XEB
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZB
Z�B
[qB
[qB
[WB
[#B
[=B
[�B
\)B
\]B
\�B
\�B
\�B
\�B
]B
]IB
]�B
^B
]�B
]�B
^OB
^jB
^�B
^�B
_B
_B
_;B
_pB
`BB
`\B
`�B
`�B
`�B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
cB
c B
c:B
c�B
c�B
c�B
c�B
d�B
d�B
e,B
ezB
ezB
e�B
f2B
f�B
f�B
f�B
g8B
gB
gB
gB
gB
gB
gB
gmB
h$B
h>B
h>B
hsB
h�B
h�B
iB
i�B
jB
j0B
jeB
j�B
j�B
kkB
k�B
k�B
k�B
lB
l�B
l�B
l�B
m)B
m)B
mwB
m�B
m�B
nB
nIB
n}B
n}B
n�B
n�B
o B
oB
oOB
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qB
qB
qAB
q'B
q�B
rGB
r-B
raB
raB
raB
r|B
raB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
t9B
tTB
tTB
tnB
t�B
u?B
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
vzB
vzB
vzB
v�B
vzB
vzB
v�B
v�B
v�B
wLB
wLB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{0B
{JB
{dB
{dB
{dB
{dB
{�B
|B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~wB
~�B
.B
cB
cB
�B
�B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�OB
�iB
��B
��B
� B
�UB
�;B
�;B
��B
��B
��B
��B
��B
��B
�'B
�B
�AB
�[B
�uB
��B
�B
�B
�-B
�B
�-B
�aB
�aB
��B
��B
��B
��B
��B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B��B�B�TB��B�B�B�B�B�B��B��B�2B��B��B�%B��B͟B�B
��B'B+�B+QB+QB)�B$tB 'BxB�B9B/B-]BF�BP�BhsB��B�B��B�YB��B��B��B�HB��BˬB�;B�B)B�zB�vB��B�B�B�BgB[	Bm�BoiBG�B%�B!�B4B"�B�B
��B
��B
��B
x8B
V�B
HfB
FYB
,�B
�B
  B	�8B	�%B	��B	��B	��B	�zB	x�B	\�B	N�B	7�B	$B	�B	�B�}B�XB�SB�XB�(B��B��B�tB�,B��B��B��B��B�B�qB��B�aB�,B�B��B�TB�B�fB�B	`�B	�JB	�fB	��B	��B	q�B	g�B	_pB	\xB	a�B	eFB	d&B	e,B	ffB	gB	n/B	s�B	v�B	u�B	o5B	e�B	LB	5tB	%,B	(�B	!-B	�B	B	�B	kB	�B	�B	EB	(�B	3hB	<�B	C�B	F?B	D�B	E�B	A�B	@B	C{B	P�B	f�B	n/B	utB	raB	qvB	�NB	��B	B	�B	�B	��B	��B	�rB	�VB
�B
 B	��B	�B
  B
 iB	��B
B
'B
 iB	��B
B
�B
�B

�B
�B
B
�B
0B
B
B
�B
B
jB
�B
�B
�B
�B
�B
B
VB
�B	�<B	��B	�DB
 �B
�B
�B
aB	�cB	��B	�lB	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�"B	��B	�QB	��B	�wB	�wB	��B	�QB	��B	�|B	�B	�B	��B	�nB	��B	��B	��B	��B	��B	�!B	��B	��B	��B	�B	�|B	�hB	�B	�vB	�iB	�qB	�QB	�B	�0B	��B	�B	�B	�B	�B	��B	�B	� B	�B	�}B	��B	�)B	�"B	�B	�)B	�IB	�B	�)B	�=B	�"B	�B	�qB	�B	�WB	�B	�6B	�B	�QB	�B	�B	�6B	�6B	�kB	�B	�B	�wB	��B	��B	�)B	�]B	�]B	�CB	��B	�B	�vB	�-B	�B	�MB	�MB	�hB	��B	�B	�B	�3B	��B	�B	�hB	�B	�MB	�B	�TB	�nB	�?B	��B	��B	��B	��B	��B	�B	�B	�RB	�RB	�	B	��B	�B	��B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�B	��B	��B	�BB	�(B	�(B	�wB	��B	�B	�cB	�cB
 B
 iB
 �B
 �B
oB
-B
-B
B
�B
GB
�B
�B
�B
�B
�B
?B
�B
�B
B
�B
KB
1B
KB
	B
�B
�B
	B
	�B
	B
	lB
	�B

rB
�B
�B
�B
�B
dB
dB
0B
JB
B
<B
VB
B
�B
[B
�B
B
�B
MB
�B
mB
�B
�B
�B
�B
�B

B
YB
YB
$B

B
�B
�B
B
�B
MB
�B
�B
�B
2B
B
SB
�B
�B
�B
�B
mB
SB
�B
SB
B
9B
B
�B
$B
�B
�B
1B
�B
QB
kB
�B
QB
�B
�B
kB
kB
7B
�B
�B
	B
�B
�B
	B
�B
�B
B
xB
�B
�B
�B
B
�B
OB
B
jB
�B
�B
�B
5B
�B
~B
�B
B
!B
!B
!B
;B
pB
�B
�B
 B
�B
 �B
 �B
 �B
!|B
!�B
"4B
"hB
"hB
"NB
"hB
# B
# B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&B
%�B
&B
&2B
&�B
&�B
'B
'�B
'�B
'�B
(
B
(�B
)*B
)*B
)DB
)B
)*B
)*B
)DB
)*B
)�B
*0B
)�B
+6B
+6B
,"B
,qB
,�B
-]B
-wB
-]B
-�B
.B
-�B
-�B
-�B
-�B
.B
./B
./B
-�B
-]B
,�B
,�B
,�B
-CB
-]B
-)B
-�B
/�B
0B
0!B
0B
0!B
0B
1'B
1B
1'B
1'B
1[B
2-B
3MB
3hB
3�B
3�B
4TB
4�B
5?B
5tB
5ZB
5tB
5�B
5�B
6+B
6FB
6`B
6`B
6`B
6FB
6�B
6�B
7B
7B
7�B
7�B
7�B
7�B
8B
8RB
8lB
8�B
9	B
9	B
9	B
9	B
9>B
9>B
9rB
9XB
9�B
9�B
9�B
9�B
:^B
:^B
:�B
;�B
;�B
<B
<�B
<�B
=�B
=qB
=�B
>BB
>�B
>�B
>�B
>�B
>�B
?.B
>�B
?HB
@iB
@iB
@�B
@�B
AB
@�B
AB
A�B
A�B
A�B
BAB
B�B
B�B
C�B
D�B
D�B
EB
EB
EmB
E�B
F%B
FtB
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IRB
J	B
JrB
J�B
K�B
K�B
L0B
L0B
L~B
L�B
LdB
MB
MjB
M�B
M�B
M�B
N<B
N�B
N�B
N�B
N�B
O�B
O�B
P.B
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T{B
T�B
UB
U2B
U2B
U2B
U�B
U�B
VB
V9B
V9B
V�B
VmB
V�B
W
B
W�B
XEB
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZB
Z�B
[qB
[qB
[WB
[#B
[=B
[�B
\)B
\]B
\�B
\�B
\�B
\�B
]B
]IB
]�B
^B
]�B
]�B
^OB
^jB
^�B
^�B
_B
_B
_;B
_pB
`BB
`\B
`�B
`�B
`�B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
cB
c B
c:B
c�B
c�B
c�B
c�B
d�B
d�B
e,B
ezB
ezB
e�B
f2B
f�B
f�B
f�B
g8B
gB
gB
gB
gB
gB
gB
gmB
h$B
h>B
h>B
hsB
h�B
h�B
iB
i�B
jB
j0B
jeB
j�B
j�B
kkB
k�B
k�B
k�B
lB
l�B
l�B
l�B
m)B
m)B
mwB
m�B
m�B
nB
nIB
n}B
n}B
n�B
n�B
o B
oB
oOB
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qB
qB
qAB
q'B
q�B
rGB
r-B
raB
raB
raB
r|B
raB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
t9B
tTB
tTB
tnB
t�B
u?B
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
vzB
vzB
vzB
v�B
vzB
vzB
v�B
v�B
v�B
wLB
wLB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{0B
{JB
{dB
{dB
{dB
{dB
{�B
|B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~wB
~�B
.B
cB
cB
�B
�B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�OB
�iB
��B
��B
� B
�UB
�;B
�;B
��B
��B
��B
��B
��B
��B
�'B
�B
�AB
�[B
�uB
��B
�B
�B
�-B
�B
�-B
�aB
�aB
��B
��B
��B
��B
��B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192252  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192252  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192252                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042259  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042259  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                