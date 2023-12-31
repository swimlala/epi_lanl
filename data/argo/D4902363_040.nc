CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-24T00:35:25Z creation;2016-09-24T00:35:28Z conversion to V3.1;2019-12-19T08:29:41Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20160924003525  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA  I2_0576_040                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5�x� 1   @��6s�� @;'l�C���dsC��%1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
B Q�B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B�B���B���B���B���B���C��C��C��C��C	��C��C��C�HC��C{C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�HCo��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�<)D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��D�B�D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�E�D�e�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�v�A�jA�bNA�XA�S�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�K�A�G�A���A�  A�VA�33A�ZA�^5A�{A�G�A�C�A��hA�p�A�~�A�&�A��A��+A�bNA�A���A�33A�1A�A��A�t�A�JA��A�\)A�n�A�$�A���A�E�A���A�t�A�ffA�?}A���A���A�G�A��A��FA�r�A�K�A�C�A�
=A��9A�7LA��!A��FA�Q�A��A�ĜA���A��wA�1A�dZA�A�A�+A��A��A���A~r�A|1'A{Az�HAw�FAt��Ar�Aq7LAo��An5?Al��Ak+Aj1Ah��Ag�7Af9XAe�Ad�9AcAa��A`E�A_?}A^�A\z�A[7LAZ-AY�AXM�AW33AV�AU�wAU\)AUVAT�jAS�^AQ�-AQ7LAP�APĜAP��APM�AO
=AM��AM?}AM�AMAL�`AMVALȴAL �AK��AJ�/AJ{AI|�AH�DAG��AF^5AE�wAC�^AB�ABM�AAƨAA\)AA?}AA�A@�RA@~�A?�A>I�A=l�A<�9A<{A;
=A:9XA9�TA9O�A8��A8�!A7��A6r�A4�`A3�;A3K�A2�`A2�9A1t�A1+A0ȴA0VA0(�A/��A.��A-�mA-"�A,~�A*�`A)�hA(��A(ZA(A'�mA'�A'?}A&1'A%ƨA%�^A%��A%�A%K�A%/A$��A$�A#�hA"�A"��A"�A!�A �/A��A"�A�An�A��A��A|�A�`A^5AO�AZA�AffAG�A7LA�A~�AbAƨA��A�/AjAbA7LA��AA`BA�9AQ�AA�yA��A	�;A	�A�A��A33A��A�RAI�A�hAr�AƨA`BA;dA�A�A|�A ^5@��@���@�;d@���@�A�@��@�%@��
@��@��@�/@�9@�1@�-@�Ĝ@���@�p�@�@�E�@��@�@�bN@�A�@�P@�V@��
@݁@� �@�E�@��@��m@�J@Ӿw@�=q@Ь@϶F@�^5@ͺ^@�+@�n�@�hs@�z�@��;@�v�@Ĵ9@�1@���@��T@�9X@�+@�~�@���@��@�j@�
=@���@�Ĝ@�(�@�|�@���@�@�z�@�K�@��+@�-@�p�@��`@�9X@�|�@���@�-@��#@���@��;@�33@�x�@���@�Q�@�@��T@�hs@��@��`@�bN@��;@�33@�ȴ@�~�@�$�@��#@�A�@���@��@���@�M�@��@���@���@�r�@�1@�
=@��@��u@��j@��@�I�@��@��`@���@���@��@���@�|�@�o@��+@���@��@�7L@�V@��`@���@��/@��@�dZ@�"�@�o@�
=@�-@�=q@�v�@�~�@���@�hs@�/@�z�@�1'@��m@��P@�+@�~�@���@��@��-@�J@��@��j@��D@�z�@�1'@��m@�ƨ@�C�@�X@��@�z�@�Q�@�1'@���@�K�@��@��H@�ȴ@�^5@�-@�$�@�{@��@��T@���@���@��u@�1@��m@��m@��
@��@�ff@�@�@���@�p�@�G�@�7L@�?}@�O�@�hs@���@��@�{@��@�@���@���@�b@��
@��@���@��@�\)@��@���@�E�@�{@�$�@�$�@��@�7L@�bN@~5?@y�#@x��@y�7@y�@y&�@yX@z��@x  @y�#@}�-@|��@|9X@{t�@{dZ@{dZ@{t�@{dZ@{C�@{33@z�\@z�@zJ@y��@yG�@xĜ@xbN@xA�@w�;@w�P@w
=@vȴ@v�+@vff@vE�@v$�@v@u��@u�h@uO�@uV@t�@tj@st�@r�H@r�!@r~�@q�@p��@pbN@o�@o�@nȴ@nff@nE�@n5?@n{@m�@m@m`B@l�/@l��@l�@l�/@l9X@kƨ@k��@kt�@kS�@k33@j�@j��@i�@i��@ihs@i7L@h��@hr�@g�w@gK�@g
=@fȴ@f��@f{@e�-@e�h@e�@e�@dz�@dI�@d(�@c��@c�F@ct�@b��@b�@a��@a��@a&�@`��@`�@`1'@_�P@_�@^��@^��@^v�@^E�@^5?@^{@]��@\�D@\9X@[�m@[�m@[�
@[�@Z�@Z��@Z�@Y��@YX@X�`@X��@XQ�@X1'@W�@W�@W��@W;d@VV@U��@Up�@U?}@UV@T��@T�/@S��@Sƨ@S��@SdZ@R�@R��@R�\@R-@Q�7@P�`@PĜ@Pr�@P1'@O��@O\)@N�y@N�@N�@N��@M��@L�j@L��@LZ@K�m@K�
@K�
@K�
@K�F@K��@Kt�@Kt�@KdZ@KC�@K33@K33@J�@J�\@JM�@Ix�@H�u@H1'@G�P@F�@F��@F�+@FV@F{@F{@F@E�@E�@E�@E�@E�@E�@E�@E��@D��@D��@D�D@DZ@DI�@D9X@D�@C�
@Cƨ@Cƨ@C�F@C��@CS�@C33@Co@Co@C@B��@B��@B�\@B^5@BM�@B-@B�@A��@A��@A�7@AG�@@�9@@r�@@ �@@  @?��@?��@?�P@?|�@?|�@?|�@?|�@?|�@?K�@>5?@=�@=O�@<�j@<z�@<I�@;��@:�@:�\@:=q@:J@9�^@9��@9hs@9X@9�@8Ĝ@8�@81'@7�@7�P@7|�@7l�@7�@6��@6v�@6ff@6V@6E�@65?@6@5@5�h@5O�@4��@4�/@4�/@4�j@4�D@4j@4Z@49X@4�@3��@2��@2n�@2^5@2^5@2-@2�@1��@1&�@1�@0��@0��@0bN@0 �@/��@/��@/l�@/+@.v�@.E�@-@-p�@-O�@,��@,��@,�j@,��@,1@+�F@+��@+��@+t�@+t�@+t�@+t�@+S�@+33@+@*�@*��@*��@*^5@)��@)��@)hs@)%@(Ĝ@(A�@'�@';d@'
=@&ȴ@&V@&{@%�@%�-@%�@%p�@%?}@%�@%V@$�j@$��@$j@$I�@$I�@$9X@$�@#�m@#��@#dZ@#C�@#@"��@"��@"��@"��@"�\@"M�@!�#@!&�@ �`@ �9@ �9@ �9@ �@ Q�@ b@   @   @��@|�@\)@K�@;d@��@��@ff@V@$�@�-@�@O�@�@��@��@�D@j@I�@(�@�@��@�F@C�@��@�!@�\@~�@n�@n�@�@�^@��@�7@X@�`@Q�@A�@A�@�@�P@l�@;d@�y@��@E�@�-@`B@?}@�@z�@(�@��@�m@�
@ƨ@��@��@S�@�H@��@~�@=q@X@&�@&�@�@�`@��@Q�@A�@ �@  @�@��@�@�P@�@ȴ@��@E�@@�h@O�@?}@/@�@V@��@��@��@�D@Z@I�@I�@1@�
@��@�@33@o@
�H@
��@
~�@
M�@
-@
�@	��@	�@	�@	�#@	�^@	��@	x�@	X@	7L@	%@��@Ĝ@��@�@bN@1'@b@b@  @�;@��@�@��@K�@
=@�y@�R@��@v�@E�@$�@$�@�@`B@/@V@�@��@�j@�@��@z�@I�@9X@1@�
@ƨ@t�@dZ@C�@o@�H@��@��@n�@-@�@�@J@��@7L@7L@&�@�@ ��@ ��@ ��@ �u@ �@ �u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�v�A�jA�bNA�XA�S�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�K�A�G�A���A�  A�VA�33A�ZA�^5A�{A�G�A�C�A��hA�p�A�~�A�&�A��A��+A�bNA�A���A�33A�1A�A��A�t�A�JA��A�\)A�n�A�$�A���A�E�A���A�t�A�ffA�?}A���A���A�G�A��A��FA�r�A�K�A�C�A�
=A��9A�7LA��!A��FA�Q�A��A�ĜA���A��wA�1A�dZA�A�A�+A��A��A���A~r�A|1'A{Az�HAw�FAt��Ar�Aq7LAo��An5?Al��Ak+Aj1Ah��Ag�7Af9XAe�Ad�9AcAa��A`E�A_?}A^�A\z�A[7LAZ-AY�AXM�AW33AV�AU�wAU\)AUVAT�jAS�^AQ�-AQ7LAP�APĜAP��APM�AO
=AM��AM?}AM�AMAL�`AMVALȴAL �AK��AJ�/AJ{AI|�AH�DAG��AF^5AE�wAC�^AB�ABM�AAƨAA\)AA?}AA�A@�RA@~�A?�A>I�A=l�A<�9A<{A;
=A:9XA9�TA9O�A8��A8�!A7��A6r�A4�`A3�;A3K�A2�`A2�9A1t�A1+A0ȴA0VA0(�A/��A.��A-�mA-"�A,~�A*�`A)�hA(��A(ZA(A'�mA'�A'?}A&1'A%ƨA%�^A%��A%�A%K�A%/A$��A$�A#�hA"�A"��A"�A!�A �/A��A"�A�An�A��A��A|�A�`A^5AO�AZA�AffAG�A7LA�A~�AbAƨA��A�/AjAbA7LA��AA`BA�9AQ�AA�yA��A	�;A	�A�A��A33A��A�RAI�A�hAr�AƨA`BA;dA�A�A|�A ^5@��@���@�;d@���@�A�@��@�%@��
@��@��@�/@�9@�1@�-@�Ĝ@���@�p�@�@�E�@��@�@�bN@�A�@�P@�V@��
@݁@� �@�E�@��@��m@�J@Ӿw@�=q@Ь@϶F@�^5@ͺ^@�+@�n�@�hs@�z�@��;@�v�@Ĵ9@�1@���@��T@�9X@�+@�~�@���@��@�j@�
=@���@�Ĝ@�(�@�|�@���@�@�z�@�K�@��+@�-@�p�@��`@�9X@�|�@���@�-@��#@���@��;@�33@�x�@���@�Q�@�@��T@�hs@��@��`@�bN@��;@�33@�ȴ@�~�@�$�@��#@�A�@���@��@���@�M�@��@���@���@�r�@�1@�
=@��@��u@��j@��@�I�@��@��`@���@���@��@���@�|�@�o@��+@���@��@�7L@�V@��`@���@��/@��@�dZ@�"�@�o@�
=@�-@�=q@�v�@�~�@���@�hs@�/@�z�@�1'@��m@��P@�+@�~�@���@��@��-@�J@��@��j@��D@�z�@�1'@��m@�ƨ@�C�@�X@��@�z�@�Q�@�1'@���@�K�@��@��H@�ȴ@�^5@�-@�$�@�{@��@��T@���@���@��u@�1@��m@��m@��
@��@�ff@�@�@���@�p�@�G�@�7L@�?}@�O�@�hs@���@��@�{@��@�@���@���@�b@��
@��@���@��@�\)@��@���@�E�@�{@�$�@�$�@��@�7L@�bN@~5?@y�#@x��@y�7@y�@y&�@yX@z��@x  @y�#@}�-@|��@|9X@{t�@{dZ@{dZ@{t�@{dZ@{C�@{33@z�\@z�@zJ@y��@yG�@xĜ@xbN@xA�@w�;@w�P@w
=@vȴ@v�+@vff@vE�@v$�@v@u��@u�h@uO�@uV@t�@tj@st�@r�H@r�!@r~�@q�@p��@pbN@o�@o�@nȴ@nff@nE�@n5?@n{@m�@m@m`B@l�/@l��@l�@l�/@l9X@kƨ@k��@kt�@kS�@k33@j�@j��@i�@i��@ihs@i7L@h��@hr�@g�w@gK�@g
=@fȴ@f��@f{@e�-@e�h@e�@e�@dz�@dI�@d(�@c��@c�F@ct�@b��@b�@a��@a��@a&�@`��@`�@`1'@_�P@_�@^��@^��@^v�@^E�@^5?@^{@]��@\�D@\9X@[�m@[�m@[�
@[�@Z�@Z��@Z�@Y��@YX@X�`@X��@XQ�@X1'@W�@W�@W��@W;d@VV@U��@Up�@U?}@UV@T��@T�/@S��@Sƨ@S��@SdZ@R�@R��@R�\@R-@Q�7@P�`@PĜ@Pr�@P1'@O��@O\)@N�y@N�@N�@N��@M��@L�j@L��@LZ@K�m@K�
@K�
@K�
@K�F@K��@Kt�@Kt�@KdZ@KC�@K33@K33@J�@J�\@JM�@Ix�@H�u@H1'@G�P@F�@F��@F�+@FV@F{@F{@F@E�@E�@E�@E�@E�@E�@E�@E��@D��@D��@D�D@DZ@DI�@D9X@D�@C�
@Cƨ@Cƨ@C�F@C��@CS�@C33@Co@Co@C@B��@B��@B�\@B^5@BM�@B-@B�@A��@A��@A�7@AG�@@�9@@r�@@ �@@  @?��@?��@?�P@?|�@?|�@?|�@?|�@?|�@?K�@>5?@=�@=O�@<�j@<z�@<I�@;��@:�@:�\@:=q@:J@9�^@9��@9hs@9X@9�@8Ĝ@8�@81'@7�@7�P@7|�@7l�@7�@6��@6v�@6ff@6V@6E�@65?@6@5@5�h@5O�@4��@4�/@4�/@4�j@4�D@4j@4Z@49X@4�@3��@2��@2n�@2^5@2^5@2-@2�@1��@1&�@1�@0��@0��@0bN@0 �@/��@/��@/l�@/+@.v�@.E�@-@-p�@-O�@,��@,��@,�j@,��@,1@+�F@+��@+��@+t�@+t�@+t�@+t�@+S�@+33@+@*�@*��@*��@*^5@)��@)��@)hs@)%@(Ĝ@(A�@'�@';d@'
=@&ȴ@&V@&{@%�@%�-@%�@%p�@%?}@%�@%V@$�j@$��@$j@$I�@$I�@$9X@$�@#�m@#��@#dZ@#C�@#@"��@"��@"��@"��@"�\@"M�@!�#@!&�@ �`@ �9@ �9@ �9@ �@ Q�@ b@   @   @��@|�@\)@K�@;d@��@��@ff@V@$�@�-@�@O�@�@��@��@�D@j@I�@(�@�@��@�F@C�@��@�!@�\@~�@n�@n�@�@�^@��@�7@X@�`@Q�@A�@A�@�@�P@l�@;d@�y@��@E�@�-@`B@?}@�@z�@(�@��@�m@�
@ƨ@��@��@S�@�H@��@~�@=q@X@&�@&�@�@�`@��@Q�@A�@ �@  @�@��@�@�P@�@ȴ@��@E�@@�h@O�@?}@/@�@V@��@��@��@�D@Z@I�@I�@1@�
@��@�@33@o@
�H@
��@
~�@
M�@
-@
�@	��@	�@	�@	�#@	�^@	��@	x�@	X@	7L@	%@��@Ĝ@��@�@bN@1'@b@b@  @�;@��@�@��@K�@
=@�y@�R@��@v�@E�@$�@$�@�@`B@/@V@�@��@�j@�@��@z�@I�@9X@1@�
@ƨ@t�@dZ@C�@o@�H@��@��@n�@-@�@�@J@��@7L@7L@&�@�@ ��@ ��@ ��@ �u@ �@ �u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uBu�BF�B33B"�B�B��B��B�B�#B��B��B�FB�B�1BiyBN�B-B$�B"�B�B6FB1'B.B"�B�B�
B��B��BB�-B�B��B��B��B��B�PB�=B�7B�B�By�BdZBH�B33B#�BbB	7BB  B
�B
�B
ǮB
�jB
�jB
�jB
�qB
�-B
��B
��B
~�B
z�B
p�B
W
B
33B
"�B
{B

=B	��B	�B	�TB	�B	��B	ƨB	�jB	�LB	�'B	��B	��B	�oB	�DB	�+B	~�B	t�B	l�B	dZB	_;B	ZB	VB	Q�B	O�B	N�B	K�B	I�B	<jB	8RB	7LB	6FB	5?B	49B	1'B	+B	(�B	(�B	+B	/B	33B	33B	-B	%�B	!�B	�B	�B	�B	bB	
=B	1B��B��B�B��B�B�B�B�B�B�yB�TB�`B�NB�/B�B��B��B��B��B��B�B��BĜB�wB�jBB��B��B��B��B��BɺBȴBĜB��B�jB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�JB�1B�B�B� B}�Bz�Bx�Bx�B~�B� B{�Bx�Bt�Br�Bm�Bl�Bl�Bl�BjBiyBhsBffBdZBdZBbNB`BB]/B\)BZBXBW
BT�BP�BM�BH�BG�BF�BC�BC�BB�BA�BA�B>wB=qB;dB:^B:^B9XB8RB49B2-B1'B1'B)�B)�B(�B&�B%�B$�B$�B#�B#�B"�B!�B�B�B�B�B�B�B�B�B�B{B{BoB\B\B\BVBVBbB\BbB\BbB\B\BoBhBoBhBhBoBbBbBbBbBoB{B�BuB�B�B�B�B�B�B�B�B�B�B�B!�B#�B$�B%�B%�B'�B(�B)�B)�B/B1'B1'B33B2-B33B5?B6FB7LB:^B<jB<jB?}BA�BC�BF�BG�BK�BM�BO�BQ�BQ�BR�BR�BR�BS�BW
BcTBffBjBy�B}�B}�B}�B�B�1B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�FB�^B��BŢBȴBɺB��B��B��B��B��B�
B��B�B�
B�B�/B�HB�ZB�`B�`B�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B	B	%B		7B	
=B	DB	PB	VB	\B	bB	hB	{B	�B	�B	"�B	#�B	%�B	&�B	+B	-B	,B	,B	,B	,B	-B	/B	2-B	33B	49B	6FB	7LB	6FB	5?B	1'B	-B	'�B	&�B	/B	7LB	9XB	9XB	K�B	E�B	J�B	^5B	`BB	_;B	cTB	gmB	hsB	jB	n�B	q�B	t�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�%B	�+B	�7B	�=B	�JB	�PB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�9B	�9B	�9B	�?B	�LB	�RB	�^B	�jB	��B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�;B	�;B	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
	7B

=B
JB
JB
PB
PB
PB
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
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
%�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
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
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
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
YB
YB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
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
^5B
^5B
^5B
_;B
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
aHB
bNB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB��BL�B9	B,=B#�B�B�]B�3B�pB�pB�B��B�-B�jBn�BSB/5B'�B%FB!|B9$B33B2GB(�B�B�B��B�BðB��B�kB��B��B��B�?B��B�)B�	B�?B��B|�Bg�BLB6zB%�BhB
	B�BuB
�B
רB
��B
��B
�B
��B
��B
�B
�B
��B
�4B
|�B
t9B
Z7B
5�B
$�B
SB
JB	��B	�B	��B	��B	�hB	�1B	�qB	��B	�3B	��B	�CB	��B	��B	�lB	��B	vB	m�B	ezB	`vB	Z�B	W
B	R�B	P}B	O�B	MPB	K�B	="B	8�B	7�B	6�B	6B	5�B	2|B	+�B	)*B	)*B	+6B	/5B	3�B	4B	-�B	&�B	"�B	�B	�B	�B	�B	�B	
=B�B��B�hB�2B�B�B�GB�cB��B��B�B�fB�TBބB�B�}BЗB�vB��BևBٴBϫB��B�HB�B�GB�B�dB�jB�^B�^BʦB��B��B��B��B�DB��B��B��B�TB�-B�TB��B��B�NB��B��B��B�B�B�jB��B�kB�_B�B�@B�NB�jB��B��B��B��B~�B|6Bz�By�B� B�oB}VBz^Bu�Bs�Bm�Bl�Bm]Bm)BkBi�BiyBgBe,BezBc:Ba-B^B]BZ�BX�BX�BV�BSBN�BI�BH�BGEBC�BD3BCaBB�BB�B?cB=�B;�B:�B;0B;0B9�B5?B3�B2�B2�B*�B+B*0B'�B&�B%�B%zB$ZB$�B$&B"�B �B \B�B�BKB�B�B�BSBB�B�B}B}BHB\B�B�B}BhB.B4BB�B�B B&BBoB�B BNB4B�B@BBBB9B�B�BKBB7B=B�B�B�B \B"NB$tB%`B&�B&�B(sB)yB*B*�B/�B1�B2GB3�B2�B49B5�B6�B7�B:�B<�B<�B@ BA�BC�BGBH1BL�BNpBPbBR:BR:BS@BSuBSuBT�BW�Bc�BfLBi�By�B~(B~B}�B��B�1B�<B�$B�~B��B�5B�B��B�;B�;B��B��B��B��B�VB�FB�B��B�B�XB��B��B�*B��B�cB�]B��B��B�iB�iB�}B�}B�UB��B��B��B�YB�B��B�B�:B�@B�[BңB�+B�gB�SB�YB�KB�~B�B�B�zB�B��B�B�B��B��B��B��B�-B�	B�6B�(B�B�.B��B	�B	tB		lB	
rB	xB	jB	pB	vB	}B	hB	FB	�B	�B	"�B	$B	&2B	'�B	+�B	-CB	,=B	,"B	,"B	,=B	-]B	/iB	2|B	3MB	4TB	6`B	7�B	6�B	5�B	2-B	.IB	(>B	&�B	/5B	7�B	9>B	9XB	K�B	EB	I�B	^jB	`vB	_�B	cnB	gmB	hsB	j�B	n�B	q�B	t�B	xB	y�B	{B	}"B	.B	�;B	�-B	�MB	�YB	�_B	�RB	�XB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�:B	�FB	�6B	�=B	�]B	�OB	�AB	�hB	�hB	�TB	�nB	�nB	�tB	��B	�lB	�^B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	��B	��B	�B	�B	�4B	�&B	�B	�B	�B	�,B	�$B	�1B	�7B	�QB	�WB	�IB	�dB	�IB	�OB	�pB	ߊB	�B	�nB	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�*B	�0B	�"B	�B	�B	�.B	�.B	�B	�HB
 B
'B
AB
AB
-B
-B
GB
gB
MB
-B
-B
-B
MB
MB
YB
	7B
	lB
	lB
	�B

�B
~B
dB
jB
PB
PB
PB
�B
�B
pB
pB
pB
vB
vB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
!�B
!�B
!�B
"�B
"�B
#B
#�B
$�B
$�B
$�B
&B
%�B
%�B
%�B
%�B
%�B
%�B
%B
&LB
($B
)*B
)DB
*B
*B
*KB
+6B
,=B
-)B
-)B
-)B
.IB
./B
./B
./B
/5B
/5B
/OB
0;B
0;B
1'B
1AB
1AB
2aB
2GB
2-B
2-B
33B
3MB
3MB
3hB
3MB
4TB
4TB
4TB
5?B
5ZB
5ZB
5ZB
5ZB
5ZB
5tB
5tB
6zB
7fB
8lB
8RB
8lB
8lB
8�B
9�B
:^B
:xB
:xB
:xB
:xB
;B
;B
;B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
O�B
QB
Q4B
RB
R B
Q�B
Q�B
RB
RB
SB
SB
SB
SB
TFB
T,B
UB
UB
UB
UB
U2B
UB
UB
VB
V9B
WYB
X+B
X+B
X+B
XEB
Y1B
Y1B
YB
YB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
[qB
\CB
\)B
\)B
\CB
\CB
\CB
]/B
]IB
]IB
]/B
]IB
]IB
]IB
^jB
^OB
^OB
_pB
_VB
_�B
`\B
`BB
`\B
`\B
abB
abB
abB
abB
abB
abB
bNB
bNB
b�B
bhB
bhB
bhB
cnB
cnB
cnB
cnB
cnB
dtB
dtB
dZB
dtB
dZB
dZB
dtB
ezB
ezB
ezB
ezB
ezB
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gmB
gmB
gmB
g�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
i�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<L��<r{�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609280036252016092800362520160928003625201806221214232018062212142320180622121423201804050407062018040504070620180405040706  JA  ARFMdecpA19c                                                                20160924093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160924003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160924003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160924003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160924003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160924003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160924003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160924003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160924003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160924003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20160924011834                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160924153247  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20160927153625  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160927153625  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190706  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031423  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                