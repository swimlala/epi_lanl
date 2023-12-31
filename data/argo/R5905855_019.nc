CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:14:04Z creation;2022-06-04T19:14:04Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604191404  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��%+<M^1   @��%wwww@.>vȴ9X�d r� Ĝ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���C  C�C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&33C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Q�@~�R@�\)@�\)A!G�AAG�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B��]B�B�B���B�B���B�(�B���B���B���B���B���B���B�(�B�\)B���B���B���B���B���B���B���B���B���B�\)B�C��C{C�GC�GC	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C&.C'��C)��C+��C-�GC/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Cp{Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D�RD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D!D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\D�D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�%�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A���A��|A���A��A��vA��A��TA��A��VA���A�oA��A��A�YA��A��A�
�A��A��A�A�
	A�
�A�A�	lA�	�A�	7A�xA�A��A�bA�A���A��fA� A�L�A�,�A�DgA��A��NAΒ�A�q�A�\)A�A�{A��KA�jKA�l"A��A�qAA�XEA��A�|�A�l�A�V�A�ŢA��]A�c�A�bNA��A��LA�YKA��#A� �A�}�A�W�A���A�|A��A�f�A�E�A��A�&�A���A��9A�j�A���A��A���A��
A��A�MA��A|V�Av��As� AoXAjy>Ah��Ag�Ac$tA^|A\l�AW�!AU/�ASGEAO]dAG�VAD1�AA-wA?��A>��A=4A;q�A:��A9/�A8K^A7˒A6�]A5j�A2�A.��A.\�A-rGA,҉A+�A*5?A*�A)�A(֡A'A%kQA#��A#W�A#�A#  A"��A"a|A!��A!�A t�A�A�Au�A'RAQ�A��A�A�A��A�A�9Am�AϫA�EAm]A�A+A��A��A�.A�!AHASA��A��A�eA�hARTA�NA��A�5AM�A��A�A��Af�Ae,AC�AK^A��AƨAa|AcA��A~�AA A�A��AOvA�WA��AHA�A
ȴA
OvA	��A	��A��A��A��A�MA~(A�AMA��A6�A��A��A��ATaA	�A�/A��AtTA �&A o@��@���@�\�@��f@�2�@��@���@���@�}�@�Y@�Ɇ@�:*@�=@�iD@��@���@��E@�=q@�k�@�=q@�iD@��H@�N�@���@��s@��@�1@�qv@��@���@��@�V@��@��@��@�/@歬@橓@��@�f�@�1@��#@�p�@�L@��@�-@�  @��@��@�~�@��U@ݲ-@�8�@�+�@�4@�1�@�*0@�&�@��@ܗ�@�tT@�'R@�\�@�C@��@�ȴ@�qv@�L0@���@ו�@�IR@ָR@պ^@��f@��'@ԕ@ӫ�@���@�-@с�@��|@�Ov@��@ϧ�@�{�@�ff@��#@�2a@�v�@��Z@�B�@�I�@ɘ�@��y@�;�@ǲ-@�[W@���@��@���@Ż0@�o @��@āo@�~@��@÷�@��@�)�@�@��@���@�%F@�B[@��@���@�B�@���@���@��)@��	@�V@���@��@�`B@��@���@�Q@���@�8�@���@���@�L0@�:*@���@�rG@��@��@���@���@��@���@�o @�(�@��@���@��@���@�͟@�|�@�N�@�	@���@���@�t�@�&�@���@�M@���@���@�zx@��@���@�]d@�C�@��]@��K@��@��D@��1@� �@��Q@���@�]�@��@�j@�K^@���@�{J@�\)@�RT@��@��`@��m@���@�~�@�YK@��@��z@��[@��@�H�@��@��'@�W�@�خ@���@�f�@��9@�:�@�ݘ@���@� i@��m@�Z�@�"h@���@�k�@��@���@���@�E�@�&�@�ԕ@��"@�k�@���@�w�@��@��^@�o�@�A @��@�=q@��@�˒@�b�@��@��m@���@�&�@���@��w@�[W@��c@�h�@���@�J#@��@��9@�`�@�ԕ@���@�q@��M@���@��e@�I�@�(�@���@�{J@�6z@���@���@�C�@��@��^@�u�@�/@�
=@��@��s@�Ĝ@���@���@�B[@��]@��z@���@���@�!�@���@�H�@�e@���@��V@�iD@�.I@�@���@�� @�YK@�R�@�7�@�ݘ@���@��P@�m]@�'�@�o@�
=@���@��@��@�7�@���@��V@�\)@�1�@�!�@��@���@��@���@��o@�W�@��;@���@��X@��n@���@�x@�+@���@�a|@�-@�{@��]@��#@�@���@�A�@��@���@���@�u%@�J�@�x@��t@�x@�>�@��@���@���@�\�@�?@���@��*@�j@�@O@�;@��@��E@��e@�`�@�7@@~�+@~!�@}Y�@}@|��@|�@{��@{.I@z�R@za|@y�T@y�-@yG�@x�p@x�@w�@wE9@v��@u��@ue,@u�@t��@tb@s�	@r��@q�@q<6@p��@p�@o�@o��@oj�@o6z@n��@nGE@m�@m`B@l�@l��@l`�@k�Q@ks@ju%@jJ@i�C@i[W@h�_@hx@g~�@g@f�8@f�M@fں@fv�@f.�@e�@e��@d��@doi@dM@cqv@cF�@b�M@b�b@bV@a�3@a��@aT�@`��@a�@a�@`��@`oi@`M@_��@_�@_�$@_K�@_o@^��@^��@^:*@]�@]��@]S&@\��@[�w@[{J@[>�@Z�c@Z5?@Y�#@Y��@Yk�@Y%@X�j@X[�@X�@W>�@Vh
@U��@U�M@U�@T��@S��@R��@R@�@Q�j@Q��@Qe,@Q�@P~(@PN�@O�A@O��@O_p@OA�@OC@N�]@N��@Nv�@Nff@N#:@M��@M8�@L��@L��@LA�@K��@K/�@Jߤ@J�\@Ju%@Jh
@J($@J �@I�@I�j@I�N@I�@H��@H|�@HD�@H�@G{J@G=@G�@F�h@F��@Fn�@F?@E�D@E��@E�'@Ek�@D��@Dz�@DA�@C��@C{J@C.I@B͟@B�@A�D@A�@A�'@A8�@@�.@@$@?��@?��@?��@?qv@?Y@>��@>n�@>+k@> �@=�)@=@=��@=^�@=B�@=%F@<��@<�U@<�@<�@;s@;�@:n�@:u@9ϫ@9�h@9N<@9&�@9@8��@8j@8 �@7�}@7n/@7@6xl@6@�@60U@5��@5Y�@5�@4��@4��@4D�@3�+@3�*@3W?@3>�@3�@2�@2=q@1�.@1�d@1�n@1(�@0�|@0��@0M@0(�@/��@/S�@/�@.�H@.�!@.��@.xl@.�@-�N@-=�@-%@,ѷ@,��@,bN@,x@+��@+��@+J#@+@*ں@*�}@*�+@*GE@)��@)��@)�X@)o @)+�@)!�@)@(��@(�@(m�@(I�@(6@(M@'�@'�6@'_p@&ȴ@&��@&u%@&_�@&B[@&6�@&_@%w2@%a�@%T�@%8�@$�)@$ �@#�@#�*@#b�@#F�@#�@"�@"�m@"�@"_�@"!�@!ϫ@!��@!hs@!5�@!�@ �@ ��@ w�@ PH@ 4n@ @�@�@a@&@�@҉@�X@�@��@}V@a|@H�@6�@�@�@��@��@f�@[W@N<@<6@%@�@M@$@�@��@/�@�@��@�,@��@@�@�@�@��@[W@B�@@@ی@�9@tT@�r@��@خ@��@��@E9@"�@�@��@s�@@�@�@�T@�3@�=@a�@<6@@@��@��@�4@�o@V�@N�@7�@!@�@C�@ i@ں@��@��@a|@4@��@�^@��@��@2a@�@�@>B@�+@��@��@�V@t�@\)@K�@C�@!-@S@�M@��@�X@�b@�\@�+@v�@YK@($@�z@��@w2@S&@:�@�@ی@ی@�@��@u�@PH@C-@>B@*�@�@  @��@��@K�@;d@
��@
�B@
�h@
�x@
��@
YK@
@	�@	�@	�d@	@	��@	��@	�n@	o @	T�@	:�@	%F@�@�?@�@w�@7�@(�@�@��@�Q@�w@b�@9�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A���A��|A���A��A��vA��A��TA��A��VA���A�oA��A��A�YA��A��A�
�A��A��A�A�
	A�
�A�A�	lA�	�A�	7A�xA�A��A�bA�A���A��fA� A�L�A�,�A�DgA��A��NAΒ�A�q�A�\)A�A�{A��KA�jKA�l"A��A�qAA�XEA��A�|�A�l�A�V�A�ŢA��]A�c�A�bNA��A��LA�YKA��#A� �A�}�A�W�A���A�|A��A�f�A�E�A��A�&�A���A��9A�j�A���A��A���A��
A��A�MA��A|V�Av��As� AoXAjy>Ah��Ag�Ac$tA^|A\l�AW�!AU/�ASGEAO]dAG�VAD1�AA-wA?��A>��A=4A;q�A:��A9/�A8K^A7˒A6�]A5j�A2�A.��A.\�A-rGA,҉A+�A*5?A*�A)�A(֡A'A%kQA#��A#W�A#�A#  A"��A"a|A!��A!�A t�A�A�Au�A'RAQ�A��A�A�A��A�A�9Am�AϫA�EAm]A�A+A��A��A�.A�!AHASA��A��A�eA�hARTA�NA��A�5AM�A��A�A��Af�Ae,AC�AK^A��AƨAa|AcA��A~�AA A�A��AOvA�WA��AHA�A
ȴA
OvA	��A	��A��A��A��A�MA~(A�AMA��A6�A��A��A��ATaA	�A�/A��AtTA �&A o@��@���@�\�@��f@�2�@��@���@���@�}�@�Y@�Ɇ@�:*@�=@�iD@��@���@��E@�=q@�k�@�=q@�iD@��H@�N�@���@��s@��@�1@�qv@��@���@��@�V@��@��@��@�/@歬@橓@��@�f�@�1@��#@�p�@�L@��@�-@�  @��@��@�~�@��U@ݲ-@�8�@�+�@�4@�1�@�*0@�&�@��@ܗ�@�tT@�'R@�\�@�C@��@�ȴ@�qv@�L0@���@ו�@�IR@ָR@պ^@��f@��'@ԕ@ӫ�@���@�-@с�@��|@�Ov@��@ϧ�@�{�@�ff@��#@�2a@�v�@��Z@�B�@�I�@ɘ�@��y@�;�@ǲ-@�[W@���@��@���@Ż0@�o @��@āo@�~@��@÷�@��@�)�@�@��@���@�%F@�B[@��@���@�B�@���@���@��)@��	@�V@���@��@�`B@��@���@�Q@���@�8�@���@���@�L0@�:*@���@�rG@��@��@���@���@��@���@�o @�(�@��@���@��@���@�͟@�|�@�N�@�	@���@���@�t�@�&�@���@�M@���@���@�zx@��@���@�]d@�C�@��]@��K@��@��D@��1@� �@��Q@���@�]�@��@�j@�K^@���@�{J@�\)@�RT@��@��`@��m@���@�~�@�YK@��@��z@��[@��@�H�@��@��'@�W�@�خ@���@�f�@��9@�:�@�ݘ@���@� i@��m@�Z�@�"h@���@�k�@��@���@���@�E�@�&�@�ԕ@��"@�k�@���@�w�@��@��^@�o�@�A @��@�=q@��@�˒@�b�@��@��m@���@�&�@���@��w@�[W@��c@�h�@���@�J#@��@��9@�`�@�ԕ@���@�q@��M@���@��e@�I�@�(�@���@�{J@�6z@���@���@�C�@��@��^@�u�@�/@�
=@��@��s@�Ĝ@���@���@�B[@��]@��z@���@���@�!�@���@�H�@�e@���@��V@�iD@�.I@�@���@�� @�YK@�R�@�7�@�ݘ@���@��P@�m]@�'�@�o@�
=@���@��@��@�7�@���@��V@�\)@�1�@�!�@��@���@��@���@��o@�W�@��;@���@��X@��n@���@�x@�+@���@�a|@�-@�{@��]@��#@�@���@�A�@��@���@���@�u%@�J�@�x@��t@�x@�>�@��@���@���@�\�@�?@���@��*@�j@�@O@�;@��@��E@��e@�`�@�7@@~�+@~!�@}Y�@}@|��@|�@{��@{.I@z�R@za|@y�T@y�-@yG�@x�p@x�@w�@wE9@v��@u��@ue,@u�@t��@tb@s�	@r��@q�@q<6@p��@p�@o�@o��@oj�@o6z@n��@nGE@m�@m`B@l�@l��@l`�@k�Q@ks@ju%@jJ@i�C@i[W@h�_@hx@g~�@g@f�8@f�M@fں@fv�@f.�@e�@e��@d��@doi@dM@cqv@cF�@b�M@b�b@bV@a�3@a��@aT�@`��@a�@a�@`��@`oi@`M@_��@_�@_�$@_K�@_o@^��@^��@^:*@]�@]��@]S&@\��@[�w@[{J@[>�@Z�c@Z5?@Y�#@Y��@Yk�@Y%@X�j@X[�@X�@W>�@Vh
@U��@U�M@U�@T��@S��@R��@R@�@Q�j@Q��@Qe,@Q�@P~(@PN�@O�A@O��@O_p@OA�@OC@N�]@N��@Nv�@Nff@N#:@M��@M8�@L��@L��@LA�@K��@K/�@Jߤ@J�\@Ju%@Jh
@J($@J �@I�@I�j@I�N@I�@H��@H|�@HD�@H�@G{J@G=@G�@F�h@F��@Fn�@F?@E�D@E��@E�'@Ek�@D��@Dz�@DA�@C��@C{J@C.I@B͟@B�@A�D@A�@A�'@A8�@@�.@@$@?��@?��@?��@?qv@?Y@>��@>n�@>+k@> �@=�)@=@=��@=^�@=B�@=%F@<��@<�U@<�@<�@;s@;�@:n�@:u@9ϫ@9�h@9N<@9&�@9@8��@8j@8 �@7�}@7n/@7@6xl@6@�@60U@5��@5Y�@5�@4��@4��@4D�@3�+@3�*@3W?@3>�@3�@2�@2=q@1�.@1�d@1�n@1(�@0�|@0��@0M@0(�@/��@/S�@/�@.�H@.�!@.��@.xl@.�@-�N@-=�@-%@,ѷ@,��@,bN@,x@+��@+��@+J#@+@*ں@*�}@*�+@*GE@)��@)��@)�X@)o @)+�@)!�@)@(��@(�@(m�@(I�@(6@(M@'�@'�6@'_p@&ȴ@&��@&u%@&_�@&B[@&6�@&_@%w2@%a�@%T�@%8�@$�)@$ �@#�@#�*@#b�@#F�@#�@"�@"�m@"�@"_�@"!�@!ϫ@!��@!hs@!5�@!�@ �@ ��@ w�@ PH@ 4n@ @�@�@a@&@�@҉@�X@�@��@}V@a|@H�@6�@�@�@��@��@f�@[W@N<@<6@%@�@M@$@�@��@/�@�@��@�,@��@@�@�@�@��@[W@B�@@@ی@�9@tT@�r@��@خ@��@��@E9@"�@�@��@s�@@�@�@�T@�3@�=@a�@<6@@@��@��@�4@�o@V�@N�@7�@!@�@C�@ i@ں@��@��@a|@4@��@�^@��@��@2a@�@�@>B@�+@��@��@�V@t�@\)@K�@C�@!-@S@�M@��@�X@�b@�\@�+@v�@YK@($@�z@��@w2@S&@:�@�@ی@ی@�@��@u�@PH@C-@>B@*�@�@  @��@��@K�@;d@
��@
�B@
�h@
�x@
��@
YK@
@	�@	�@	�d@	@	��@	��@	�n@	o @	T�@	:�@	%F@�@�?@�@w�@7�@(�@�@��@�Q@�w@b�@9�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	~�B	~�B	~]B	~�B	~�B	~wB	~wB	~]B	~�B	~BB	~BB	~BB	~(B	~B	~(B	~(B	~(B	~(B	~(B	~B	~BB	~BB	~wB	~BB	~wB	~wB	~wB	~BB	~]B	~BB	~BB	~B	}qB	|jB	z�B	y�B	q�B	v`B	��B	�B	��B	��B	�B
�B
J	B
�B
��B
��B
�B+B�B$�B)�B5ZB7�B=BQhBmwBpB]dBB�B8�B,"B+BCB�BB
�B
�yB
�9B
āB
��B
��B
|jB
��B
�`B
��B
z�B
\B
<jB
4�B
./B
B	�xB	�+B	��B	�B	Y1B	J	B	?�B	*B	�B	%B�LB�zB��B�&BοB��B�B��B��B�-B�'B��B�!B�B��B�oB��B��B��B�B�.B�}B�-B�aB�{BĶBÖB�4B��B�B�?BȚB�	B�"B�B҉B�yB�;B��B�>B�B�B�B�RB	 iB	+B	1B	B	!B	�B	B	 �B	(�B	6�B	=�B	CaB	MPB	O�B	R�B	\�B	h�B	n}B	r�B	o�B	j�B	d�B	b�B	eFB	iDB	l�B	ezB	g�B	kB	j�B	n�B	|�B	��B	��B	�VB	�B	��B	�B	��B	�xB	�jB	��B	��B	��B	��B	�B	��B	��B	ĶB	ňB	�%B	��B	ƨB	ňB	��B	�%B	ƨB	�%B	ŢB	��B	ǔB	��B	�KB	��B	�B	��B	ȀB	ȚB	�RB	�XB	�	B	ɺB	�lB	�=B	�^B	ʌB	�#B	ʌB	ʦB	�=B	�	B	�	B	ʦB	˒B	�DB	�)B	��B	ʌB	��B	οB	�B	�dB	�B	ϫB	��B	��B	յB	��B	�B	ՁB	�B	�BB	�B	�%B	��B	��B	��B	�oB	�#B	��B	�7B	��B	�EB	�3B	�3B	��B	͹B	��B	�^B	�XB	�lB	�RB	�B	�vB	��B	�hB	уB	�hB	уB	ѷB	�hB	�B	�9B	�$B	�1B	�_B	ؓB	��B	��B	�_B	��B	��B	�YB	�sB	��B	�7B	�KB	ؓB	�B	��B	��B	�=B	�	B	ۦB	�B	޸B	��B	�BB	��B	߾B	�;B	�B	�vB	�-B	��B	�B	�4B	� B	�B	�tB	�ZB	�B	�B	��B	�B	�B	�8B	�8B	�mB	��B	�sB	�sB	�yB	�B	�_B	�B	�DB	�DB	��B	�KB	��B	�_B	��B	�B	�0B	�B	�B	�B	�)B	�B	��B	� B	��B	�B	�;B	�!B	�oB	�B	�B	�B	�B	��B	��B	�-B	�B	�aB	�-B	�B	�%B	��B	�%B	��B	�B	�FB	��B	�fB	��B	��B	��B	�B	�RB	�B	��B	��B	�^B	�xB	�*B	��B	��B	�PB	��B	�B	�dB	��B	�*B	�rB	�XB	�rB	��B	��B	��B	��B	�*B	��B	�B	�B	��B	�<B	��B	��B	�(B	��B	�}B	�HB	��B	�}B	�B	�B	�.B	��B	�}B
 4B
�B
�B
uB
�B
�B
�B
3B
�B
�B
9B
mB
�B
�B
%B
�B
1B
�B
	B
	7B
	7B
	RB

#B

	B

=B

�B
�B
�B
~B
�B
�B
PB
�B
�B
�B
�B
\B
(B
�B
HB
�B
NB
�B
B
�B
oB
�B
�B
&B
@B
B
,B
�B
�B
MB
�B
�B
mB
�B
�B
$B
YB
YB
�B
_B
�B
B
B
1B
�B
7B
�B
�B
�B
�B
#B
=B
WB
�B
B
)B
�B
)B
�B
�B
�B
�B
�B
~B
�B
~B
IB
B
�B
�B
!B
�B
 B
 'B
 'B
 vB
 �B
!B
!HB
!|B
"B
"B
"NB
"hB
"�B
"�B
#TB
$@B
$�B
%FB
%`B
%zB
%�B
%�B
&fB
'B
'8B
'B
'mB
'�B
(
B
(>B
(�B
(�B
)�B
*eB
*KB
*eB
*�B
*�B
+QB
,B
,�B
-CB
-�B
-�B
-�B
.B
./B
.IB
/5B
/�B
0B
0oB
0oB
0�B
0�B
0�B
0�B
1B
0�B
1[B
2|B
2�B
2-B
2-B
2�B
2�B
3�B
4B
4B
4�B
4�B
4�B
5%B
5�B
5%B
5%B
5�B
5tB
5ZB
5�B
5�B
6�B
7fB
7�B
7�B
7�B
88B
8RB
8�B
9	B
9	B
9�B
9�B
9�B
9�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<�B
<�B
="B
=�B
=�B
>]B
>wB
>�B
?}B
@�B
@iB
@iB
A;B
A;B
A B
A B
AoB
AoB
AoB
A�B
AoB
AUB
AB
@�B
A B
A�B
BB
A�B
BAB
B�B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
C�B
C�B
DB
DMB
EB
E�B
E�B
FYB
FYB
FtB
GB
G+B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
I7B
IRB
IlB
IlB
I�B
I�B
I�B
I�B
J	B
J�B
KxB
K�B
KDB
KxB
K�B
L0B
L~B
L�B
L�B
MB
MPB
MjB
M�B
M�B
MjB
M�B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
OvB
O\B
OvB
O�B
O�B
O�B
P.B
PHB
P�B
Q B
Q4B
Q�B
Q�B
R B
RoB
SB
SB
SB
S&B
S[B
TB
T{B
T{B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VSB
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W�B
XB
X�B
X�B
X�B
YB
YeB
YeB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[qB
[�B
[qB
\)B
\CB
\xB
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
_;B
_;B
_;B
_�B
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
aHB
abB
a�B
bB
b4B
bNB
bhB
b�B
b�B
cB
c B
cnB
c�B
c�B
c�B
c�B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
e,B
e`B
ezB
ezB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
j�B
kB
k6B
kQB
kQB
kkB
kkB
k�B
k�B
l"B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
nB
ncB
n}B
ncB
o B
oiB
o�B
o�B
o�B
o�B
o�B
p;B
p;B
poB
p�B
p�B
p�B
qB
qB
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
r|B
r�B
r�B
sB
s3B
shB
shB
s�B
s�B
s�B
tB
tB
t�B
uB
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wB
w2B
w�B
w�B
w�B
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y$B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~]B
~]B
~�B
~�B
~�B
~�B
.B
�B
�B
�OB
�iB
�iB
��B
��B
�B
� B
�UB
��B
��B
��B
��B
��B
��B
�[B
�'B
�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	~�B	~�B	~]B	~�B	~�B	~wB	~wB	~]B	~�B	~BB	~BB	~BB	~(B	~B	~(B	~(B	~(B	~(B	~(B	~B	~BB	~BB	~wB	~BB	~wB	~wB	~wB	~BB	~]B	~BB	~BB	~B	}qB	|jB	z�B	y�B	q�B	v`B	��B	�B	��B	��B	�B
�B
J	B
�B
��B
��B
�B+B�B$�B)�B5ZB7�B=BQhBmwBpB]dBB�B8�B,"B+BCB�BB
�B
�yB
�9B
āB
��B
��B
|jB
��B
�`B
��B
z�B
\B
<jB
4�B
./B
B	�xB	�+B	��B	�B	Y1B	J	B	?�B	*B	�B	%B�LB�zB��B�&BοB��B�B��B��B�-B�'B��B�!B�B��B�oB��B��B��B�B�.B�}B�-B�aB�{BĶBÖB�4B��B�B�?BȚB�	B�"B�B҉B�yB�;B��B�>B�B�B�B�RB	 iB	+B	1B	B	!B	�B	B	 �B	(�B	6�B	=�B	CaB	MPB	O�B	R�B	\�B	h�B	n}B	r�B	o�B	j�B	d�B	b�B	eFB	iDB	l�B	ezB	g�B	kB	j�B	n�B	|�B	��B	��B	�VB	�B	��B	�B	��B	�xB	�jB	��B	��B	��B	��B	�B	��B	��B	ĶB	ňB	�%B	��B	ƨB	ňB	��B	�%B	ƨB	�%B	ŢB	��B	ǔB	��B	�KB	��B	�B	��B	ȀB	ȚB	�RB	�XB	�	B	ɺB	�lB	�=B	�^B	ʌB	�#B	ʌB	ʦB	�=B	�	B	�	B	ʦB	˒B	�DB	�)B	��B	ʌB	��B	οB	�B	�dB	�B	ϫB	��B	��B	յB	��B	�B	ՁB	�B	�BB	�B	�%B	��B	��B	��B	�oB	�#B	��B	�7B	��B	�EB	�3B	�3B	��B	͹B	��B	�^B	�XB	�lB	�RB	�B	�vB	��B	�hB	уB	�hB	уB	ѷB	�hB	�B	�9B	�$B	�1B	�_B	ؓB	��B	��B	�_B	��B	��B	�YB	�sB	��B	�7B	�KB	ؓB	�B	��B	��B	�=B	�	B	ۦB	�B	޸B	��B	�BB	��B	߾B	�;B	�B	�vB	�-B	��B	�B	�4B	� B	�B	�tB	�ZB	�B	�B	��B	�B	�B	�8B	�8B	�mB	��B	�sB	�sB	�yB	�B	�_B	�B	�DB	�DB	��B	�KB	��B	�_B	��B	�B	�0B	�B	�B	�B	�)B	�B	��B	� B	��B	�B	�;B	�!B	�oB	�B	�B	�B	�B	��B	��B	�-B	�B	�aB	�-B	�B	�%B	��B	�%B	��B	�B	�FB	��B	�fB	��B	��B	��B	�B	�RB	�B	��B	��B	�^B	�xB	�*B	��B	��B	�PB	��B	�B	�dB	��B	�*B	�rB	�XB	�rB	��B	��B	��B	��B	�*B	��B	�B	�B	��B	�<B	��B	��B	�(B	��B	�}B	�HB	��B	�}B	�B	�B	�.B	��B	�}B
 4B
�B
�B
uB
�B
�B
�B
3B
�B
�B
9B
mB
�B
�B
%B
�B
1B
�B
	B
	7B
	7B
	RB

#B

	B

=B

�B
�B
�B
~B
�B
�B
PB
�B
�B
�B
�B
\B
(B
�B
HB
�B
NB
�B
B
�B
oB
�B
�B
&B
@B
B
,B
�B
�B
MB
�B
�B
mB
�B
�B
$B
YB
YB
�B
_B
�B
B
B
1B
�B
7B
�B
�B
�B
�B
#B
=B
WB
�B
B
)B
�B
)B
�B
�B
�B
�B
�B
~B
�B
~B
IB
B
�B
�B
!B
�B
 B
 'B
 'B
 vB
 �B
!B
!HB
!|B
"B
"B
"NB
"hB
"�B
"�B
#TB
$@B
$�B
%FB
%`B
%zB
%�B
%�B
&fB
'B
'8B
'B
'mB
'�B
(
B
(>B
(�B
(�B
)�B
*eB
*KB
*eB
*�B
*�B
+QB
,B
,�B
-CB
-�B
-�B
-�B
.B
./B
.IB
/5B
/�B
0B
0oB
0oB
0�B
0�B
0�B
0�B
1B
0�B
1[B
2|B
2�B
2-B
2-B
2�B
2�B
3�B
4B
4B
4�B
4�B
4�B
5%B
5�B
5%B
5%B
5�B
5tB
5ZB
5�B
5�B
6�B
7fB
7�B
7�B
7�B
88B
8RB
8�B
9	B
9	B
9�B
9�B
9�B
9�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<�B
<�B
="B
=�B
=�B
>]B
>wB
>�B
?}B
@�B
@iB
@iB
A;B
A;B
A B
A B
AoB
AoB
AoB
A�B
AoB
AUB
AB
@�B
A B
A�B
BB
A�B
BAB
B�B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
C�B
C�B
DB
DMB
EB
E�B
E�B
FYB
FYB
FtB
GB
G+B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
I7B
IRB
IlB
IlB
I�B
I�B
I�B
I�B
J	B
J�B
KxB
K�B
KDB
KxB
K�B
L0B
L~B
L�B
L�B
MB
MPB
MjB
M�B
M�B
MjB
M�B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
OvB
O\B
OvB
O�B
O�B
O�B
P.B
PHB
P�B
Q B
Q4B
Q�B
Q�B
R B
RoB
SB
SB
SB
S&B
S[B
TB
T{B
T{B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VSB
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W�B
XB
X�B
X�B
X�B
YB
YeB
YeB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[qB
[�B
[qB
\)B
\CB
\xB
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
_;B
_;B
_;B
_�B
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
aHB
abB
a�B
bB
b4B
bNB
bhB
b�B
b�B
cB
c B
cnB
c�B
c�B
c�B
c�B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
e,B
e`B
ezB
ezB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
j�B
kB
k6B
kQB
kQB
kkB
kkB
k�B
k�B
l"B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
nB
ncB
n}B
ncB
o B
oiB
o�B
o�B
o�B
o�B
o�B
p;B
p;B
poB
p�B
p�B
p�B
qB
qB
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
r|B
r�B
r�B
sB
s3B
shB
shB
s�B
s�B
s�B
tB
tB
t�B
uB
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
wB
w2B
w�B
w�B
w�B
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y$B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~]B
~]B
~�B
~�B
~�B
~�B
.B
�B
�B
�OB
�iB
�iB
��B
��B
�B
� B
�UB
��B
��B
��B
��B
��B
��B
�[B
�'B
�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105230  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191404  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191404  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191404                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041411  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041411  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                