CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-04-12T10:00:27Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200412100027  20200412100027  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4744                            2B  A   NAVIS_A                         0188                            052512                          863 @�Ws|1   @�X$��@1��t�j�e!�^5?}1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C{C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C4{C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�
>C��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߼)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�ĜA�G�A���A��mA��#A���A�ĜA�ĜAͮAͩ�A͟�A͓uA͋DA�z�A�^5A�;dA�-A�
=A�t�A�{A�A���A�bA��A�A˥�A˟�A˺^AˬA˟�A˃A�p�A�n�A�33A��A��Aʴ9Aʇ+A�A�ȴA�Aɥ�A�XA��A�  A��A��A�ƨAȧ�AȬAȧ�AȋDA�`BA�33A�$�A�A���A��A��TA���AǺ^Aǥ�A�|�A�t�A�C�A���AƶFA�Q�A���Aŉ7A�(�A�l�A©�A�p�A�&�A���A���A�ffA�G�A�+A�/A� �A��A�t�A�$�A���A���A�;dA�VA��A�  A�"�A�-A���A��-A���A�7LA�/A�I�A���A���A�(�A���A��mA��`A��`A��-A�r�A�ĜA���A��A�;dA�A��A���A�C�A�$�A�7LA��\A~M�A|^5A{��Ay��Au�At(�Ar{ApZAo\)Am�
AkA_&�AV�AS�ARM�AP�AK?}AH5?AF-AC�AA&�A@n�A>��A;�7A:�yA:��A:�!A:ZA:(�A9�FA933A8�RA7�FA7;dA5�#A/��A.�A-dZA+�mA*�A)?}A(�A%�A%l�A%�A%&�A$��A$�DA#�
A"1'A!+A!VA �A �uA!t�A!��A �A�
A\)A33AS�A�wA E�A �uA ��A ��AE�A��A��A\)A�A��A9XA�7A�A��A�RA�A�wA��A��AƨAl�AG�A�A�`A�HA��A��A�uA~�Az�A5?A|�AVA%A�/An�A�A�Al�AZAp�A
��A	|�Ar�A��AO�A�jA��A�+A33A�hAG�A �A �DA I�@��H@��-@�G�@�z�@��@�o@���@��^@��@��`@�b@�"�@���@�=q@�`B@��@��@��@�+@��@�Q�@�{@�7L@�A�@�
=@�=q@���@�bN@�C�@�M�@�p�@��@�  @�J@ڸR@�&�@��@�z�@���@ם�@��;@� �@�X@�&�@��
@���@�M�@ٲ-@�x�@�?}@��@��@��/@؃@�(�@��;@׮@�l�@�ƨ@�1'@���@���@�=q@���@��#@��#@պ^@Ձ@�O�@�7L@���@�Z@��
@Ӆ@�\)@�K�@�;d@�"�@�~�@Гu@���@�O�@�G�@�G�@�?}@�7L@�/@�&�@��@��@��/@�r�@�  @��@��`@ȃ@�I�@��@ǍP@�"�@��@�ff@��T@ŉ7@��/@�1@�33@�v�@�@�hs@�&�@��`@���@�j@�9X@���@���@�ȴ@��!@���@���@�~�@�ff@�5?@��@��-@�V@��D@� �@�ƨ@��@�+@�=q@�%@��@��m@��m@��;@��;@��;@��
@��@���@�\)@�+@�@���@�V@���@���@�V@�  @��;@��
@�|�@�;d@���@�E�@��#@���@��@�&�@��j@�bN@�Q�@�Q�@�Q�@�A�@��@�+@�ȴ@��!@�ff@�5?@���@���@��h@�X@��9@��u@�z�@�Z@�A�@�9X@�(�@�(�@��y@���@���@��^@�x�@�V@��u@�  @��;@��F@�|�@�33@�+@�@�5?@���@��@��T@���@�@�`B@��@��@��D@�bN@�bN@�Z@�Q�@�I�@�I�@�I�@�I�@�I�@�Q�@�Q�@�Z@�Z@�Q�@� �@��@��;@��
@�ƨ@��@��P@�t�@�\)@�C�@�o@�hs@��u@�r�@�Z@�9X@��@�  @��@��;@��;@��
@��w@��P@�"�@���@��@��\@�$�@��-@�O�@�Z@��@��@���@��7@�p�@�X@�?}@�&�@��@��/@��@��@�j@�A�@��@��m@��P@�C�@��@���@�5?@��@�t�@���@��\@��+@��+@�n�@�@���@��-@���@���@���@���@��7@��7@�p�@�`B@�X@�?}@��@��`@�Ĝ@�bN@���@�+@��y@���@�E�@�-@��T@�p�@�`B@�/@�&�@�&�@��@��@��@��@���@���@��j@�I�@� �@��@�l�@�33@��@���@��!@��@��-@�x�@�G�@���@��j@��@�9X@��@�1@���@���@���@���@�~�@�ff@�M�@�M�@�=q@���@���@�&�@�Q�@��@�+@��@�o@�
=@���@��y@���@��+@�J@��#@���@�X@���@�z�@�b@��@��@�P@\)@~ȴ@~$�@}��@}`B@}?}@}V@|��@|Z@|1@{dZ@{@y�^@yG�@y&�@xĜ@x  @w;d@v��@v�y@v�+@v5?@v@t�/@t�@r�@rJ@qx�@q7L@q&�@q�@p��@pA�@o�@o;d@n�y@n�@nE�@m�-@m�h@m`B@l�@l�/@l��@l9X@kt�@j~�@i�#@i��@iG�@i&�@i�@h�`@g�;@f��@fȴ@fv�@fV@f$�@e�T@d�/@c��@cS�@co@c@b�!@a��@ahs@aG�@`��@`A�@_�@_�P@_|�@_|�@_|�@_\)@^��@^$�@]p�@\�/@\�D@\I�@\1@[��@[t�@[@Z=q@Y�#@Y�^@Y�^@YG�@Y%@Y%@X��@X�`@XbN@X  @W�w@V��@V�+@U�@UO�@T��@T�D@Tj@S�
@SS�@R��@R^5@R�@Q��@Qx�@Q%@O�;@O|�@N��@M@L��@L�@K�m@K��@J�@J~�@JM�@I�#@HQ�@G�P@G|�@F�@Fv�@E@E`B@E�@D��@D��@D�j@Dj@DZ@DZ@DZ@DI�@C��@C��@C�@Ct�@B��@A�@Ax�@Ax�@A7L@@Ĝ@@�u@@�u@@r�@@ �@?��@?�@?�P@?
=@>��@>E�@=�@=@=�h@=�@=p�@=O�@<��@<�j@<�@<Z@;t�@;C�@;o@;@:�H@:�!@:�\@:^5@:J@97L@9�@8�`@8��@8�u@8r�@81'@7�;@7|�@7+@6�R@6ff@6{@5�@5�T@5��@5@5�h@5O�@5V@4�D@4I�@4(�@41@3��@3S�@333@3"�@3"�@2�@2�!@2�\@2~�@2~�@2n�@2n�@2n�@2-@1�#@1X@1&�@0�`@0��@0r�@0Q�@0  @/�@/+@.��@.5?@.{@-�@-�-@-?}@-V@,�@,��@,�j@,��@,��@,��@,I�@,�@+�
@+��@+dZ@*�H@*~�@)��@)G�@)�@(�9@(r�@(A�@(1'@(  @'��@'�w@'�@'K�@&�y@&�@&E�@&$�@%�T@%�h@%�@%�@%�@%p�@%p�@%p�@%O�@%O�@%�@$�@$�/@$�@$j@$9X@#�
@#t�@#"�@#@"�H@"��@"��@"��@"�!@"�\@"M�@"=q@"=q@"=q@"-@!��@!�7@!x�@!G�@!�@!%@ �`@ ��@�;@�w@�@�@\)@
=@�y@�y@ȴ@v�@�@�h@p�@O�@O�@/@/@�@�@�j@�D@z�@j@Z@9X@�@��@�F@��@@n�@^5@^5@M�@-@�@�@J@��@��@G�@r�@bN@1'@�;@l�@;d@�@�@
=@
=@�y@ȴ@��@�+@$�@{@�h@�@I�@�@1@1@��@��@��@��@�m@�F@@��@�\@=q@�#@��@��@�7@�7@�7@x�@hs@hs@hs@X@G�@%@�@�w@|�@ȴ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�ĜA�G�A���A��mA��#A���A�ĜA�ĜAͮAͩ�A͟�A͓uA͋DA�z�A�^5A�;dA�-A�
=A�t�A�{A�A���A�bA��A�A˥�A˟�A˺^AˬA˟�A˃A�p�A�n�A�33A��A��Aʴ9Aʇ+A�A�ȴA�Aɥ�A�XA��A�  A��A��A�ƨAȧ�AȬAȧ�AȋDA�`BA�33A�$�A�A���A��A��TA���AǺ^Aǥ�A�|�A�t�A�C�A���AƶFA�Q�A���Aŉ7A�(�A�l�A©�A�p�A�&�A���A���A�ffA�G�A�+A�/A� �A��A�t�A�$�A���A���A�;dA�VA��A�  A�"�A�-A���A��-A���A�7LA�/A�I�A���A���A�(�A���A��mA��`A��`A��-A�r�A�ĜA���A��A�;dA�A��A���A�C�A�$�A�7LA��\A~M�A|^5A{��Ay��Au�At(�Ar{ApZAo\)Am�
AkA_&�AV�AS�ARM�AP�AK?}AH5?AF-AC�AA&�A@n�A>��A;�7A:�yA:��A:�!A:ZA:(�A9�FA933A8�RA7�FA7;dA5�#A/��A.�A-dZA+�mA*�A)?}A(�A%�A%l�A%�A%&�A$��A$�DA#�
A"1'A!+A!VA �A �uA!t�A!��A �A�
A\)A33AS�A�wA E�A �uA ��A ��AE�A��A��A\)A�A��A9XA�7A�A��A�RA�A�wA��A��AƨAl�AG�A�A�`A�HA��A��A�uA~�Az�A5?A|�AVA%A�/An�A�A�Al�AZAp�A
��A	|�Ar�A��AO�A�jA��A�+A33A�hAG�A �A �DA I�@��H@��-@�G�@�z�@��@�o@���@��^@��@��`@�b@�"�@���@�=q@�`B@��@��@��@�+@��@�Q�@�{@�7L@�A�@�
=@�=q@���@�bN@�C�@�M�@�p�@��@�  @�J@ڸR@�&�@��@�z�@���@ם�@��;@� �@�X@�&�@��
@���@�M�@ٲ-@�x�@�?}@��@��@��/@؃@�(�@��;@׮@�l�@�ƨ@�1'@���@���@�=q@���@��#@��#@պ^@Ձ@�O�@�7L@���@�Z@��
@Ӆ@�\)@�K�@�;d@�"�@�~�@Гu@���@�O�@�G�@�G�@�?}@�7L@�/@�&�@��@��@��/@�r�@�  @��@��`@ȃ@�I�@��@ǍP@�"�@��@�ff@��T@ŉ7@��/@�1@�33@�v�@�@�hs@�&�@��`@���@�j@�9X@���@���@�ȴ@��!@���@���@�~�@�ff@�5?@��@��-@�V@��D@� �@�ƨ@��@�+@�=q@�%@��@��m@��m@��;@��;@��;@��
@��@���@�\)@�+@�@���@�V@���@���@�V@�  @��;@��
@�|�@�;d@���@�E�@��#@���@��@�&�@��j@�bN@�Q�@�Q�@�Q�@�A�@��@�+@�ȴ@��!@�ff@�5?@���@���@��h@�X@��9@��u@�z�@�Z@�A�@�9X@�(�@�(�@��y@���@���@��^@�x�@�V@��u@�  @��;@��F@�|�@�33@�+@�@�5?@���@��@��T@���@�@�`B@��@��@��D@�bN@�bN@�Z@�Q�@�I�@�I�@�I�@�I�@�I�@�Q�@�Q�@�Z@�Z@�Q�@� �@��@��;@��
@�ƨ@��@��P@�t�@�\)@�C�@�o@�hs@��u@�r�@�Z@�9X@��@�  @��@��;@��;@��
@��w@��P@�"�@���@��@��\@�$�@��-@�O�@�Z@��@��@���@��7@�p�@�X@�?}@�&�@��@��/@��@��@�j@�A�@��@��m@��P@�C�@��@���@�5?@��@�t�@���@��\@��+@��+@�n�@�@���@��-@���@���@���@���@��7@��7@�p�@�`B@�X@�?}@��@��`@�Ĝ@�bN@���@�+@��y@���@�E�@�-@��T@�p�@�`B@�/@�&�@�&�@��@��@��@��@���@���@��j@�I�@� �@��@�l�@�33@��@���@��!@��@��-@�x�@�G�@���@��j@��@�9X@��@�1@���@���@���@���@�~�@�ff@�M�@�M�@�=q@���@���@�&�@�Q�@��@�+@��@�o@�
=@���@��y@���@��+@�J@��#@���@�X@���@�z�@�b@��@��@�P@\)@~ȴ@~$�@}��@}`B@}?}@}V@|��@|Z@|1@{dZ@{@y�^@yG�@y&�@xĜ@x  @w;d@v��@v�y@v�+@v5?@v@t�/@t�@r�@rJ@qx�@q7L@q&�@q�@p��@pA�@o�@o;d@n�y@n�@nE�@m�-@m�h@m`B@l�@l�/@l��@l9X@kt�@j~�@i�#@i��@iG�@i&�@i�@h�`@g�;@f��@fȴ@fv�@fV@f$�@e�T@d�/@c��@cS�@co@c@b�!@a��@ahs@aG�@`��@`A�@_�@_�P@_|�@_|�@_|�@_\)@^��@^$�@]p�@\�/@\�D@\I�@\1@[��@[t�@[@Z=q@Y�#@Y�^@Y�^@YG�@Y%@Y%@X��@X�`@XbN@X  @W�w@V��@V�+@U�@UO�@T��@T�D@Tj@S�
@SS�@R��@R^5@R�@Q��@Qx�@Q%@O�;@O|�@N��@M@L��@L�@K�m@K��@J�@J~�@JM�@I�#@HQ�@G�P@G|�@F�@Fv�@E@E`B@E�@D��@D��@D�j@Dj@DZ@DZ@DZ@DI�@C��@C��@C�@Ct�@B��@A�@Ax�@Ax�@A7L@@Ĝ@@�u@@�u@@r�@@ �@?��@?�@?�P@?
=@>��@>E�@=�@=@=�h@=�@=p�@=O�@<��@<�j@<�@<Z@;t�@;C�@;o@;@:�H@:�!@:�\@:^5@:J@97L@9�@8�`@8��@8�u@8r�@81'@7�;@7|�@7+@6�R@6ff@6{@5�@5�T@5��@5@5�h@5O�@5V@4�D@4I�@4(�@41@3��@3S�@333@3"�@3"�@2�@2�!@2�\@2~�@2~�@2n�@2n�@2n�@2-@1�#@1X@1&�@0�`@0��@0r�@0Q�@0  @/�@/+@.��@.5?@.{@-�@-�-@-?}@-V@,�@,��@,�j@,��@,��@,��@,I�@,�@+�
@+��@+dZ@*�H@*~�@)��@)G�@)�@(�9@(r�@(A�@(1'@(  @'��@'�w@'�@'K�@&�y@&�@&E�@&$�@%�T@%�h@%�@%�@%�@%p�@%p�@%p�@%O�@%O�@%�@$�@$�/@$�@$j@$9X@#�
@#t�@#"�@#@"�H@"��@"��@"��@"�!@"�\@"M�@"=q@"=q@"=q@"-@!��@!�7@!x�@!G�@!�@!%@ �`@ ��@�;@�w@�@�@\)@
=@�y@�y@ȴ@v�@�@�h@p�@O�@O�@/@/@�@�@�j@�D@z�@j@Z@9X@�@��@�F@��@@n�@^5@^5@M�@-@�@�@J@��@��@G�@r�@bN@1'@�;@l�@;d@�@�@
=@
=@�y@ȴ@��@�+@$�@{@�h@�@I�@�@1@1@��@��@��@��@�m@�F@@��@�\@=q@�#@��@��@�7@�7@�7@x�@hs@hs@hs@X@G�@%@�@�w@|�@ȴ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
$�B
 �B
�B
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
uB
bB
bB
uB
�B
!�B
(�B
-B
/B
9XB
9XB
33B
49B
9XB
C�B
C�B
C�B
?}B
A�B
N�B
M�B
F�B
F�B
C�B
A�B
49B
49B
8RB
:^B
49B
2-B
33B
6FB
9XB
9XB
=qB
F�B
H�B
M�B
ZB
aHB
bNB
iyB
n�B
r�B
u�B
y�B
� B
�B
�+B
�=B
��B
�3B
��B
��B
�B
��BDB"�BG�BjBq�B�B��B�-B�LB�9B�B�-BɺB�ZB�B�B�B�B�B�B�B�B��B�B�HB�B��BÖB�B��B��B��B��B��B��B��B��By�BgmB]/BVB>wB1B
�B
�B
�hB
e`B
R�B
33B
	7B	��B	�B	�5B	ŢB	�qB	�3B	��B	��B	��B	{�B	@�B	�B	JB	%B��B�B�fB�BB�)B�B��B��B��B��B��B��BɺBȴBǮBƨBĜBB�wB�RB�jB�XB�LB�FB�?B�?B�LB��BĜBǮBɺBɺB��B��B�#B�B�B�B��B	{B	�B	\B		7B	+B	JB	{B	�B	&�B	,B	33B	D�B	F�B	C�B	E�B	D�B	E�B	D�B	C�B	A�B	A�B	@�B	?}B	A�B	B�B	D�B	B�B	>wB	<jB	<jB	<jB	;dB	=qB	=qB	@�B	A�B	A�B	A�B	A�B	A�B	@�B	?}B	A�B	A�B	?}B	=qB	;dB	<jB	7LB	1'B	%�B	�B	�B	�B	uB		7B��B�B�`B�fB�sB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	%B	DB	JB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	$�B	)�B	,B	,B	+B	,B	1'B	6FB	:^B	E�B	n�B	q�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	r�B	s�B	s�B	r�B	t�B	v�B	}�B	�%B	�1B	�=B	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�\B	�bB	�hB	�oB	�uB	�uB	�uB	�uB	�uB	�uB	��B	�B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�wB	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
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
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
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
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
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
v�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
$�B
 �B
�B
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
uB
bB
bB
uB
�B
!�B
(�B
-B
/B
9XB
9XB
33B
49B
9XB
C�B
C�B
C�B
?}B
A�B
N�B
M�B
F�B
F�B
C�B
A�B
49B
49B
8RB
:^B
49B
2-B
33B
6FB
9XB
9XB
=qB
F�B
H�B
M�B
ZB
aHB
bNB
iyB
n�B
r�B
u�B
y�B
� B
�B
�+B
�=B
��B
�3B
��B
��B
�B
��BDB"�BG�BjBq�B�B��B�-B�LB�9B�B�-BɺB�ZB�B�B�B�B�B�B�B�B��B�B�HB�B��BÖB�B��B��B��B��B��B��B��B��By�BgmB]/BVB>wB1B
�B
�B
�hB
e`B
R�B
33B
	7B	��B	�B	�5B	ŢB	�qB	�3B	��B	��B	��B	{�B	@�B	�B	JB	%B��B�B�fB�BB�)B�B��B��B��B��B��B��BɺBȴBǮBƨBĜBB�wB�RB�jB�XB�LB�FB�?B�?B�LB��BĜBǮBɺBɺB��B��B�#B�B�B�B��B	{B	�B	\B		7B	+B	JB	{B	�B	&�B	,B	33B	D�B	F�B	C�B	E�B	D�B	E�B	D�B	C�B	A�B	A�B	@�B	?}B	A�B	B�B	D�B	B�B	>wB	<jB	<jB	<jB	;dB	=qB	=qB	@�B	A�B	A�B	A�B	A�B	A�B	@�B	?}B	A�B	A�B	?}B	=qB	;dB	<jB	7LB	1'B	%�B	�B	�B	�B	uB		7B��B�B�`B�fB�sB�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	%B	DB	JB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	$�B	)�B	,B	,B	+B	,B	1'B	6FB	:^B	E�B	n�B	q�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	r�B	s�B	s�B	r�B	t�B	v�B	}�B	�%B	�1B	�=B	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�\B	�bB	�hB	�oB	�uB	�uB	�uB	�uB	�uB	�uB	��B	�B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�wB	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
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
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
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
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
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
v�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200412100027                              AO  ARCAADJP                                                                    20200412100027    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200412100027  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200412100027  QCF$                G�O�G�O�G�O�0               