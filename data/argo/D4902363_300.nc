CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-13T00:36:39Z creation;2018-11-13T00:36:44Z conversion to V3.1;2019-12-19T07:28:12Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20181113003639  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              ,A   JA  I2_0576_300                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؐ5��#�1   @ؐ6����@8��	k���d.i�B��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ Dʼ�D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր Dּ�D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D��3D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	�HC��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>{C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5xRD5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʼ)D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�<)D�|)Dտ\D��\D�?\D�\Dּ)D��\D�<)D�|)D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D܂�Dܿ\D��)D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D߂�D�D��\D�?\D�\D�\D��\D�?\D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AˬAˣ�A˰!AˮA˰!A˰!A˰!A˲-A˴9A˴9A˸RA˸RA˾wA���A˼jA˶FA˸RA˸RA˺^A˼jA˼jA�A�ȴA���A���A���A���A˗�A��;AżjAĬA�M�A���A�-A�JA�z�A�K�A��yA�^5A��DA�M�A��hA��A���A��\A��/A��A�7LA�^5A��A�33A��A���A���A���A�ZA�$�A��+A���A���A��mA�`BA�%A���A�bA��RA�r�A��!A��PA�&�A���A���A���A�33A�v�A�%A���A��A���A��A���A��;A��hA�VA��^A��A�K�A�1'A�S�A�/A�JA�A��A�-A�bA��^A���A�l�A�O�A���A��A�M�A�JA�VA/A~jA|�A{O�Az5?Aw�wAwl�Aw�Au;dAr�AqC�Ap^5Ao��An��Amx�Ak�#AjffAh��Ag;dAf�DAd�Ac��AcdZAc+AbVA^M�A\��A[��AZ��AZ{AYK�AX��AW�AT~�ASC�AR~�ARJAQ33AP{AO
=AM�wAM`BAM/ALffAKhsAJ��AI�
AG�hAEAC
=AB9XAA��AA"�A@��A@(�A?�A>�A=�#A=�7A<�!A:ĜA9p�A8��A7��A6Q�A5l�A4��A4I�A2{A0Q�A0 �A0  A/�#A/�PA/7LA.�!A.M�A-�#A-S�A,I�A,A+��A+XA*�A*�\A)t�A'�;A'oA%��A$-A"��A!��A ^5AG�AA�AM�A��A��A �A�TA��A�hA+A��A�uA��AA�A�wA7LA��A�AC�Az�A�;A��Ax�AƨA$�A�/A5?A|�A
=A
�A	�A	l�A�HA��AVA�7A
=A��A�A/A�
AS�AM�A �@��@�J@���@��/@�b@�C�@���@�1'@�V@�o@�?}@��@@���@��@띲@�`B@�D@��
@���@��@�@�w@�@�\@�Z@���@ܓu@�b@�|�@�dZ@���@�$�@ٺ^@��@�j@��`@���@ҏ\@���@�j@��
@͡�@̋D@˾w@�C�@�
=@��y@���@�5?@ǍP@�{@ź^@��`@���@�C�@+@�?}@��j@��@��w@��@���@���@�?}@�1'@��@�33@�E�@���@��@�(�@�|�@���@�@��^@���@�Z@��@�ȴ@��@�O�@��@���@���@���@���@�dZ@�+@��@��R@���@�^5@�J@��h@�z�@�I�@�V@���@��@�$�@�%@���@�J@�1'@�^5@��@��@�Z@��w@�l�@��y@��@��@��9@���@���@�r�@�(�@�
=@��\@�~�@�E�@��@���@�Ĝ@��F@��!@�^5@���@��7@�hs@�&�@��@��u@��@�  @���@�
=@��+@�-@�{@�{@�{@��@�hs@�7L@�&�@��@��@��/@���@��D@� �@���@���@�t�@�;d@��H@���@�E�@���@�X@�V@��j@�bN@���@�t�@���@���@��@�J@��@��@���@��@���@��@�O�@�%@���@��D@�z�@�r�@�Z@�I�@��@�@��@��y@���@��!@��+@�^5@���@���@�`B@�O�@�G�@�7L@�/@�V@���@��`@���@��@+@~ȴ@~��@~�+@~ff@~{@}@}�h@}�h@}�@}O�@|�/@|��@|z�@|j@|Z@{��@{��@{t�@{"�@z��@z~�@zn�@z-@z�@y��@y�7@x��@xbN@xA�@x  @w��@w�@w�P@w|�@w|�@w|�@w|�@wl�@wl�@wl�@wK�@w�@w
=@v�y@vȴ@v�+@u�T@u�h@u�@t��@t�/@t�@tz�@tj@t�@s��@s�F@sC�@r�\@q%@o�;@n��@nV@n$�@m�@mV@l�@l�j@l��@lz�@l9X@l1@k��@kC�@j~�@i�#@iX@i%@h��@h �@g��@g�@hA�@g��@g+@fV@ep�@e�@d�j@d��@d�@c�F@ct�@b�H@b-@a��@`��@_�@_�@_\)@_
=@^��@^�R@^��@^��@^��@^ff@^5?@]�-@]�h@]�h@]p�@]O�@]/@\�@\�/@\�@\z�@\j@\Z@\I�@\9X@\(�@\1@[��@[o@ZJ@YX@Y%@X�@X  @Wl�@V�@V{@U��@Up�@U�@T�/@T��@T��@S�F@S"�@S@R�@R��@R�\@R^5@R=q@RJ@RJ@QG�@P��@O��@O;d@Nff@NE�@N5?@M�@M�@L��@LI�@L9X@L�@L�@K��@K�m@K�m@K�F@K�@Kt�@Kt�@Kt�@KdZ@KC�@KC�@K"�@J�@J�H@J��@J��@J��@Jn�@J-@JJ@I�^@I��@IG�@H�9@Hr�@HQ�@H �@Gl�@E�T@E�@D�/@D��@Dj@DZ@DZ@DZ@D9X@D(�@C��@C��@C�@CdZ@B�@B�\@B^5@BM�@A�@A�^@A��@AX@A&�@@Ĝ@@��@@�u@@A�@?�@?K�@>�R@>ff@>{@=@=�-@=�h@<�/@<�j@<��@<(�@;��@;�m@;�m@;�m@;�m@;�m@;�m@;�m@;��@;��@;�m@;dZ@:~�@:J@9G�@8 �@7��@6�R@6{@5�T@5�T@5@5�@4��@4�j@3ƨ@333@2=q@1�^@1G�@0��@0�9@0A�@/|�@/
=@.�y@.�+@-�@-��@-@-�-@-��@-`B@-?}@-�@+��@+33@*�@*��@*��@*�!@*�!@*�!@*~�@*�@)�@)hs@)%@(��@(r�@(A�@(b@'��@'�@'��@'l�@'
=@&��@&V@%@%�h@%�@%`B@$��@$��@$�@$��@$�D@$z�@$Z@$Z@$I�@$(�@#�m@#�m@#��@#�@#dZ@"�H@"n�@"=q@!��@!7L@ r�@��@l�@
=@�+@E�@5?@$�@�@@�-@�@p�@p�@O�@?}@�/@��@j@I�@9X@�@�@��@�m@�m@�m@�m@�m@ƨ@t�@C�@o@��@n�@-@��@��@X@�@�9@�@Q�@A�@ �@b@  @  @�@��@�@��@|�@�P@|�@|�@l�@l�@l�@l�@\)@+@
=@��@ȴ@�+@E�@@�@�T@�-@��@p�@?}@/@�@V@�/@�j@I�@��@�m@��@t�@33@"�@o@o@o@@�@�@�H@��@��@M�@-@-@-@-@=q@-@�@��@�^@��@x�@X@X@X@G�@7L@7L@7L@7L@�@��@��@�@bN@bN@bN@r�@A�@��@\)@K�@K�@;d@;d@+@�y@��@�+@�+@v�@ff@V@$�@�@�T@@�h@/@z�@9X@9X@1@�
@��@"�@
��@
�!@
��@
~�@
~�@
n�@
M�@
J@	��@	��@	x�@	X@	%@��@��@�@�@�@r�@r�@ �@�;@�P@+@ȴ@�+@v�@ff@E�@{@��@�@`B@�@�j@Z@9X@(�@�@�m@dZ@C�@"�@o@o@@�@�H@��@�!@M�@�@�#@��@��@�7@ �`@ r�@ bN@ bN@ bN@ bN@ bN@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AˬAˣ�A˰!AˮA˰!A˰!A˰!A˲-A˴9A˴9A˸RA˸RA˾wA���A˼jA˶FA˸RA˸RA˺^A˼jA˼jA�A�ȴA���A���A���A���A˗�A��;AżjAĬA�M�A���A�-A�JA�z�A�K�A��yA�^5A��DA�M�A��hA��A���A��\A��/A��A�7LA�^5A��A�33A��A���A���A���A�ZA�$�A��+A���A���A��mA�`BA�%A���A�bA��RA�r�A��!A��PA�&�A���A���A���A�33A�v�A�%A���A��A���A��A���A��;A��hA�VA��^A��A�K�A�1'A�S�A�/A�JA�A��A�-A�bA��^A���A�l�A�O�A���A��A�M�A�JA�VA/A~jA|�A{O�Az5?Aw�wAwl�Aw�Au;dAr�AqC�Ap^5Ao��An��Amx�Ak�#AjffAh��Ag;dAf�DAd�Ac��AcdZAc+AbVA^M�A\��A[��AZ��AZ{AYK�AX��AW�AT~�ASC�AR~�ARJAQ33AP{AO
=AM�wAM`BAM/ALffAKhsAJ��AI�
AG�hAEAC
=AB9XAA��AA"�A@��A@(�A?�A>�A=�#A=�7A<�!A:ĜA9p�A8��A7��A6Q�A5l�A4��A4I�A2{A0Q�A0 �A0  A/�#A/�PA/7LA.�!A.M�A-�#A-S�A,I�A,A+��A+XA*�A*�\A)t�A'�;A'oA%��A$-A"��A!��A ^5AG�AA�AM�A��A��A �A�TA��A�hA+A��A�uA��AA�A�wA7LA��A�AC�Az�A�;A��Ax�AƨA$�A�/A5?A|�A
=A
�A	�A	l�A�HA��AVA�7A
=A��A�A/A�
AS�AM�A �@��@�J@���@��/@�b@�C�@���@�1'@�V@�o@�?}@��@@���@��@띲@�`B@�D@��
@���@��@�@�w@�@�\@�Z@���@ܓu@�b@�|�@�dZ@���@�$�@ٺ^@��@�j@��`@���@ҏ\@���@�j@��
@͡�@̋D@˾w@�C�@�
=@��y@���@�5?@ǍP@�{@ź^@��`@���@�C�@+@�?}@��j@��@��w@��@���@���@�?}@�1'@��@�33@�E�@���@��@�(�@�|�@���@�@��^@���@�Z@��@�ȴ@��@�O�@��@���@���@���@���@�dZ@�+@��@��R@���@�^5@�J@��h@�z�@�I�@�V@���@��@�$�@�%@���@�J@�1'@�^5@��@��@�Z@��w@�l�@��y@��@��@��9@���@���@�r�@�(�@�
=@��\@�~�@�E�@��@���@�Ĝ@��F@��!@�^5@���@��7@�hs@�&�@��@��u@��@�  @���@�
=@��+@�-@�{@�{@�{@��@�hs@�7L@�&�@��@��@��/@���@��D@� �@���@���@�t�@�;d@��H@���@�E�@���@�X@�V@��j@�bN@���@�t�@���@���@��@�J@��@��@���@��@���@��@�O�@�%@���@��D@�z�@�r�@�Z@�I�@��@�@��@��y@���@��!@��+@�^5@���@���@�`B@�O�@�G�@�7L@�/@�V@���@��`@���@��@+@~ȴ@~��@~�+@~ff@~{@}@}�h@}�h@}�@}O�@|�/@|��@|z�@|j@|Z@{��@{��@{t�@{"�@z��@z~�@zn�@z-@z�@y��@y�7@x��@xbN@xA�@x  @w��@w�@w�P@w|�@w|�@w|�@w|�@wl�@wl�@wl�@wK�@w�@w
=@v�y@vȴ@v�+@u�T@u�h@u�@t��@t�/@t�@tz�@tj@t�@s��@s�F@sC�@r�\@q%@o�;@n��@nV@n$�@m�@mV@l�@l�j@l��@lz�@l9X@l1@k��@kC�@j~�@i�#@iX@i%@h��@h �@g��@g�@hA�@g��@g+@fV@ep�@e�@d�j@d��@d�@c�F@ct�@b�H@b-@a��@`��@_�@_�@_\)@_
=@^��@^�R@^��@^��@^��@^ff@^5?@]�-@]�h@]�h@]p�@]O�@]/@\�@\�/@\�@\z�@\j@\Z@\I�@\9X@\(�@\1@[��@[o@ZJ@YX@Y%@X�@X  @Wl�@V�@V{@U��@Up�@U�@T�/@T��@T��@S�F@S"�@S@R�@R��@R�\@R^5@R=q@RJ@RJ@QG�@P��@O��@O;d@Nff@NE�@N5?@M�@M�@L��@LI�@L9X@L�@L�@K��@K�m@K�m@K�F@K�@Kt�@Kt�@Kt�@KdZ@KC�@KC�@K"�@J�@J�H@J��@J��@J��@Jn�@J-@JJ@I�^@I��@IG�@H�9@Hr�@HQ�@H �@Gl�@E�T@E�@D�/@D��@Dj@DZ@DZ@DZ@D9X@D(�@C��@C��@C�@CdZ@B�@B�\@B^5@BM�@A�@A�^@A��@AX@A&�@@Ĝ@@��@@�u@@A�@?�@?K�@>�R@>ff@>{@=@=�-@=�h@<�/@<�j@<��@<(�@;��@;�m@;�m@;�m@;�m@;�m@;�m@;�m@;��@;��@;�m@;dZ@:~�@:J@9G�@8 �@7��@6�R@6{@5�T@5�T@5@5�@4��@4�j@3ƨ@333@2=q@1�^@1G�@0��@0�9@0A�@/|�@/
=@.�y@.�+@-�@-��@-@-�-@-��@-`B@-?}@-�@+��@+33@*�@*��@*��@*�!@*�!@*�!@*~�@*�@)�@)hs@)%@(��@(r�@(A�@(b@'��@'�@'��@'l�@'
=@&��@&V@%@%�h@%�@%`B@$��@$��@$�@$��@$�D@$z�@$Z@$Z@$I�@$(�@#�m@#�m@#��@#�@#dZ@"�H@"n�@"=q@!��@!7L@ r�@��@l�@
=@�+@E�@5?@$�@�@@�-@�@p�@p�@O�@?}@�/@��@j@I�@9X@�@�@��@�m@�m@�m@�m@�m@ƨ@t�@C�@o@��@n�@-@��@��@X@�@�9@�@Q�@A�@ �@b@  @  @�@��@�@��@|�@�P@|�@|�@l�@l�@l�@l�@\)@+@
=@��@ȴ@�+@E�@@�@�T@�-@��@p�@?}@/@�@V@�/@�j@I�@��@�m@��@t�@33@"�@o@o@o@@�@�@�H@��@��@M�@-@-@-@-@=q@-@�@��@�^@��@x�@X@X@X@G�@7L@7L@7L@7L@�@��@��@�@bN@bN@bN@r�@A�@��@\)@K�@K�@;d@;d@+@�y@��@�+@�+@v�@ff@V@$�@�@�T@@�h@/@z�@9X@9X@1@�
@��@"�@
��@
�!@
��@
~�@
~�@
n�@
M�@
J@	��@	��@	x�@	X@	%@��@��@�@�@�@r�@r�@ �@�;@�P@+@ȴ@�+@v�@ff@E�@{@��@�@`B@�@�j@Z@9X@(�@�@�m@dZ@C�@"�@o@o@@�@�H@��@�!@M�@�@�#@��@��@�7@ �`@ r�@ bN@ bN@ bN@ bN@ bN@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�sB�yB�sB�yB�yB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BȴBG�BB�BhsB��B��B�1B��B�?B��B��B��B��B��B�hB��B�PB}�BhsB^5BbNBl�Bl�BZB<jBD�B;dB�B!�B�B�B�B�B��B��B��B�B�B�5BȴB�3B��B�bB�VB�{B�Bl�B_;BO�B8RB �B�BPBJB	7B
��B
�B
�`B
��B
��B
B
��B
��B
ŢB
�9B
��B
��B
��B
��B
��B
��B
�oB
�oB
�1B
z�B
m�B
q�B
dZB
YB
VB
@�B
E�B
>wB
,B
{B
�B
�B
{B
	7B
  B	�B	�B	�mB	�BB	�;B	�B	��B	��B	ɺB	�RB	��B	�\B	��B	�=B	�7B	�B	w�B	iyB	N�B	\)B	[#B	[#B	Q�B	I�B	B�B	@�B	C�B	B�B	8RB	-B	+B	�B	+B��B��B		7B	1B	+B	B��B��B�B��B�B�ZB�B��B��B��BɺBƨB��BƨB�?B�'BÖBĜBB��B�qB�XB�XB�FB�3B�B�-B�'B�B�B��B��B�PB�{B�+B�Bx�Bw�Bn�Bp�Bz�Bz�Bs�Bq�Bo�Bl�Bt�Br�Bs�Bn�Bl�BgmB]/BVB_;B]/B^5BR�BN�BN�BK�BB�B:^B7LB33B<jB?}BA�BA�BA�B@�BA�BA�BC�BA�B;dB<jB<jB8RB7LB0!B7LB2-B)�B.B49B:^B8RB49B33B,B$�B"�B�B#�B'�B,B&�B&�B(�B�B&�B)�B&�B(�B$�B'�B(�B'�B�B�B+B2-B2-B7LB6FB33B5?B1'B,B�B)�B33B2-B(�B,B%�B,B1'B49B6FB6FB49B.B$�B-B:^B6FB5?B8RB8RB5?B<jB>wB<jB=qB:^BC�BB�B>wBD�BB�BA�BD�BC�BB�BC�BH�BG�BJ�BF�BL�BJ�BM�BM�BR�BP�B\)BdZBe`BdZBdZBe`BgmBjBk�BjBjBjBiyBq�BiyBgmBm�Bl�Bs�Bw�Bv�By�By�B�B�+B�JB�JB�\B�\B�JB��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�dB�wBBƨBƨBƨB��B��B��B��B��B��B�B�#B�/B�/B�#B�)B�TB�`B�`B�`B�mB�mB�mB�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	1B	JB	PB	PB	
=B	oB	$�B	%�B	%�B	&�B	'�B	-B	/B	2-B	49B	7LB	8RB	7LB	7LB	5?B	:^B	D�B	H�B	J�B	L�B	O�B	Q�B	R�B	XB	]/B	`BB	aHB	aHB	aHB	`BB	`BB	`BB	^5B	^5B	[#B	bNB	gmB	hsB	hsB	iyB	m�B	p�B	s�B	s�B	t�B	v�B	{�B	|�B	}�B	}�B	}�B	� B	�B	�B	�+B	�7B	�DB	�DB	�PB	�VB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�B	�9B	�FB	�dB	�jB	�dB	�qB	��B	��B	��B	B	B	B	B	B	B	ĜB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�/B	�BB	�NB	�ZB	�ZB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
+B
1B
	7B
	7B

=B

=B

=B
	7B

=B

=B
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
PB
PB
PB
PB
PB
PB
PB
JB
VB
VB
PB
DB
	7B
VB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
 �B
�B
"�B
 �B
 �B
$�B
#�B
&�B
(�B
)�B
(�B
(�B
'�B
'�B
&�B
(�B
'�B
+B
,B
,B
-B
-B
,B
.B
0!B
/B
/B
2-B
2-B
2-B
2-B
2-B
33B
2-B
0!B
33B
7LB
8RB
9XB
9XB
9XB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
;dB
<jB
<jB
=qB
>wB
@�B
?}B
?}B
?}B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
D�B
F�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
R�B
R�B
S�B
T�B
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
XB
YB
YB
YB
YB
ZB
ZB
[#B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
]/B
\)B
]/B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
aHB
bNB
bNB
bNB
bNB
aHB
aHB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
cTB
e`B
e`B
e`B
e`B
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
ffB
ffB
ffB
ffB
e`B
e`B
hsB
iyB
iyB
hsB
iyB
hsB
iyB
k�B
l�B
k�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
t�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
w�B
w�B
w�B
v�B
u�B
v�B
y�B
y�B
y�B
y�B
y�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�sB�_B�sB�yB�yB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�'B� B�TBJ�BI7Bl�B��B��B�6B�B�`B��B��B�IB�hB�VB�[B�xB�BB��BlqBbBe,BnBm�B]/BABGB>�B!�B#nB_B��B��B��B��B��B��B��B�B�\B˒B��B�sB��B��B��B�EBo�Ba�BRTB;�B$tB�BBBB
	B
�qB
�UB
�mB
՛B
��B
��B
�B
�B
�YB
�B
�4B
��B
�DB
�fB
�NB
��B
�@B
�B
�7B
|�B
oiB
r�B
f2B
[#B
W�B
CB
F?B
?}B
.cB
YB
eB
�B
MB

�B
�B	��B	�|B	�DB	�4B	�vB	��B	�0B	�BB	�rB	�DB	��B	�hB	�sB	��B	�#B	�'B	x�B	kQB	R:B	]�B	\)B	[�B	S&B	K)B	C�B	A�B	DB	CB	9rB	.cB	+�B	xB	
#B	B�6B	
#B		B	�B	�B	  B�B�B�2B�[B��B�_BѝB�B�PB�^B��BˬB��B�B��B��B��B��B�B�B�B��B��B�B�OB��B��B��B��B��B�OB�vB��B�7B��Bz�ByrBp�Bq�B{JB{dBt�Br�Bp�BmwBuBsBtBo5BmBh>B^�BW�B_�B^B^�BTFBPBO�BL�BD3B<jB9�B5tB>B@�BBuBBABB[BAUBB[BBABDBBAB<�B=<B="B9rB8�B1�B88B3�B+�B/�B4�B:�B8�B4�B3�B-)B&LB$tB�B%,B)B,�B(
B'�B)�B!-B'�B*�B'�B)�B%�B(�B)�B(�BVB/B+�B2�B2�B7�B6�B3�B5�B1�B-B!B+B3�B2�B*B,�B'mB,�B1�B4�B6zB6zB4�B.�B&�B.B:�B6�B5�B8�B8�B6+B<�B>�B=B>B;dBC�BCB?cBD�BC-BBABE9BDBCGBD3BI7BHKBK)BG�BMBK�BN�BN�BS�BQ�B\]BdZBezBd�Bd�Be�Bg�Bj�Bk�Bj�Bj�BkBjeBr-BkBh�Bn}Bm�Bt�Bx�BxB{0B{0B��B��B��B��B��B��B�PB��B��B��B��B��B�;B�~B�2B�"B�kB�QB�kB��B��B��B��B��B��B��B��B�B�B�B�DB�<B�HB�[B�EB�=B�IB�IB�WBܒB�nB�zB�zB�B�B�B�B��B��B��B��B��B��B�B��B�B�B�B�*B�]B	 �B	fB	JB	�B	�B	
�B	�B	$�B	%�B	%�B	'B	(>B	-CB	/OB	2aB	4�B	7fB	8lB	7fB	7�B	5�B	:�B	D�B	H�B	J�B	MB	PB	R B	S@B	X_B	]dB	`\B	abB	abB	abB	`\B	`\B	`vB	^�B	^�B	[�B	bhB	g�B	h�B	h�B	i�B	m�B	p�B	s�B	s�B	t�B	v�B	|B	}B	~B	~B	~(B	�4B	�-B	�SB	�EB	�RB	�^B	�^B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�$B	�>B	�B	�=B	�/B	�5B	�5B	�UB	�UB	�;B	�AB	�[B	�[B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	ªB	ªB	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�:B	�[B	�FB	�+B	�EB	�7B	�EB	�QB	�WB	�qB	یB	�~B	ݘB	�vB	�B	�tB	�tB	�zB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�*B	�B	��B	�B	�B	�B	�B	�.B	�B	�.B	�6B	�(B	�PB
 4B
 iB
B
3B
aB
MB
gB
EB
fB
	lB
	7B

XB

=B

=B
	RB

XB

=B
DB
JB
dB
dB
PB
~B
dB
PB
jB
jB
jB
jB
jB
jB
jB
jB
jB
�B
�B
pB
�B
�B
	�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
 �B
 B
#B
!B
!HB
%B
$&B
'B
)B
)�B
)B
)*B
($B
($B
'RB
)*B
(XB
+6B
,=B
,=B
-)B
-CB
,qB
.IB
0;B
/OB
/iB
2GB
2-B
2-B
2GB
2aB
3MB
2aB
0�B
3�B
7fB
8�B
9XB
9XB
9XB
8lB
8lB
7�B
7fB
7�B
8lB
8�B
9rB
:xB
:xB
;B
;B
<�B
<�B
;�B
<�B
<�B
=�B
>�B
@�B
?�B
?�B
?�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
D�B
F�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
N�B
NB
N�B
N�B
N�B
N�B
O�B
Q B
Q B
RB
SB
SB
T,B
UB
VB
V9B
W$B
W
B
W$B
W
B
W$B
W$B
XB
XEB
YB
YB
YB
YB
Y1B
YB
YB
YB
XEB
Y1B
YKB
Y1B
Y1B
ZQB
Z7B
[#B
\)B
[=B
\]B
\CB
\CB
]/B
]/B
]IB
\]B
]IB
\]B
]IB
^jB
^OB
_VB
_pB
`\B
`BB
`\B
`BB
`BB
`BB
`BB
`BB
`BB
`\B
_�B
aHB
bNB
bNB
bNB
bhB
abB
abB
`\B
aHB
abB
a|B
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bhB
bhB
abB
cTB
cTB
cTB
cTB
cTB
bhB
a�B
cnB
ezB
e`B
e`B
ezB
d�B
dtB
dtB
ezB
f�B
ffB
ffB
f�B
ezB
f�B
f�B
f�B
f�B
e�B
e�B
h�B
iyB
i�B
h�B
i�B
h�B
i�B
k�B
l�B
k�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
t�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
w�B
w�B
xB
v�B
vB
v�B
y�B
y�B
y�B
y�B
y�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811170034522018111700345220181117003452201811170200162018111702001620181117020016201811180032492018111800324920181118003249  JA  ARFMdecpA19c                                                                20181113093636  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181113003639  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181113003642  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181113003642  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181113003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181113003643  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181113003643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181113003643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181113003644  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181113003644                      G�O�G�O�G�O�                JA  ARUP                                                                        20181113005617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181113153410  CV  JULD            G�O�G�O�Fā�                JM  ARCAJMQC2.0                                                                 20181116153452  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181116153452  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181116170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181117153249  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                