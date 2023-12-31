CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-16T00:36:23Z creation;2018-11-16T00:36:28Z conversion to V3.1;2019-12-19T07:27:58Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181116003623  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              -A   JA  I2_0576_301                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؐ��]��1   @ؐ���� @8�MjO�d-�PH1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cd{Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D(D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DLDL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�DiDi~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\D�D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�B�D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʡ�Aʣ�Aʣ�Aʥ�Aʥ�Aʧ�Aʣ�Aʣ�Aʥ�Aʧ�Aʥ�Aʧ�Aʥ�Aʣ�Aʡ�Aʡ�Aʟ�Aʡ�Aʣ�Aʣ�Aʟ�Aʥ�Aʥ�Aʟ�Aʛ�AʋDA�\)A�{A��HA�hsA���Aȏ\A��TA���A�JA�%A��DA��`A� �A���A��mA�%A���A�(�A���A�33A��A�~�A��wA�v�A�~�A��9A�A�jA��A��A���A�O�A�A�A��A�ZA�oA���A���A�=qA���A�/A�n�A�Q�A��-A�z�A���A�33A�G�A��hA�VA�
=A��A�|�A�n�A�jA�`BA�O�A�oA��mA�A�|�A��9A�%A���A� �A�p�A���A��A��yA���A�jA�v�A���A��TA�5?A�oA��At�A~9XA|^5A{�
A{�A{K�Az�`Az(�Awx�Au\)As�Ap�!Ao��An�DAnA�Am��Am�-Amp�Am7LAl�AkG�AjffAiXAhr�Ag"�Ae�AcS�AbbAa�;Aa��A`��A_�FA^1A]hsA\�uAZ�AZbAX�AXQ�AW�AV�AUx�AT{AS?}AS�AR��AR(�AQ`BAPz�AN~�ANAM33AK��AI�
AIoAG�TAF�`AEx�AD�AC��AC
=AAXA@�9A@A�A>��A=x�A<$�A;�A;S�A9hsA8��A85?A7�FA7XA733A6��A5�A4��A4M�A3�mA3�^A2ȴA1��A01A.=qA.�A.bA.1A-��A,��A,{A+&�A)�wA(bA&n�A%�;A%��A%�A%;dA%�A%%A$��A$�yA$�HA$�!A$ffA#;dA"�/A"��A"��A!l�A ��A�wAG�A��AC�A��AffA��AS�A��AbNA�A��A33A^5A�^AM�AdZAz�A  A��A�\A��A
bA�+AƨA�A?}A�A��A�7A��A�PA"�A�^A�Av�At�A ��@��@��T@�I�@��@�dZ@�5?@�Ĝ@��@�~�@���@���@�t�@���@�&�@�Ĝ@�\)@�u@�^5@�
=@�u@�;d@���@��@��D@�  @��m@��@�E�@�J@�&�@���@ԣ�@�r�@�1'@��@���@ӥ�@�C�@���@�ff@�@�7L@��`@�\)@��#@�/@��`@�1'@��;@˕�@ʸR@���@�$�@š�@�&�@���@ă@�Q�@� �@å�@�S�@��y@�E�@�?}@�r�@���@��@���@��T@���@��@�dZ@��@���@��T@���@�(�@�ƨ@�"�@���@��^@��9@� �@�"�@�-@���@�b@��
@��P@��R@��@�7L@�A�@�;d@�V@���@�~�@�5?@���@�1@��H@�n�@�E�@�{@��7@���@�Q�@��@��+@��@�@�?}@�V@��j@�A�@�K�@��@��+@�^5@�=q@�$�@�{@���@���@�X@� �@��
@��F@��@�l�@��@���@��@���@�v�@�E�@�$�@�@�@���@�G�@���@��j@���@�o@�5?@���@��h@�`B@�/@�/@�r�@���@�"�@���@�n�@�M�@�5?@�$�@���@���@���@��7@�p�@�`B@�G�@���@�j@��m@��@�+@�@�ȴ@���@�~�@�$�@��@���@�{@�@�@��7@�`B@��@��@��u@�(�@��m@��@�l�@�33@��y@�n�@�M�@�-@�{@���@��@��#@��#@���@��^@�p�@��/@��@��u@�I�@�1'@�(�@�(�@�(�@�(�@� �@��@��@�b@�  @\)@}�h@|�D@|z�@|I�@{�m@{ƨ@{ƨ@{ƨ@{��@{t�@{S�@{C�@{"�@z�@z��@z~�@z�@y��@y��@y��@y�7@y�@x�9@xr�@xQ�@xb@w�P@wK�@v�+@v@u�T@u@u��@u�@up�@up�@t��@t�@s�
@s��@sS�@r��@qx�@pĜ@pr�@p  @o�@n�R@m��@l��@l��@lz�@kdZ@j��@j^5@j�@i��@i�#@i�#@i��@i��@i�^@i�^@ix�@hQ�@g��@gl�@f�y@f��@fȴ@f��@f��@fȴ@f�y@f�y@f�R@f$�@e�h@e�@dI�@c�
@c�@b��@b-@bJ@a�^@a��@a�7@aG�@a&�@`��@`Ĝ@`�u@`A�@`b@`b@`b@`  @_�w@_K�@^ff@^@]�T@]��@\�@\j@\1@[33@Y�^@YX@X�`@XQ�@W
=@VE�@U�h@T9X@SdZ@R�@R�!@R��@R�!@R~�@R-@Q��@Q��@Qhs@Q7L@PĜ@Pr�@P1'@P  @O�;@O�@O�P@O\)@OK�@O;d@O+@O
=@N��@N�y@N��@Nff@M�@M�@L�D@Lz�@LZ@K��@K��@KC�@J�H@I�@Ihs@HĜ@H�u@H�@HA�@H �@G�;@Gl�@F�y@Fv�@F{@E��@E�h@Ep�@E/@EV@D�/@D�@D�@C�m@C�F@C�@C@A��@Ahs@@��@@��@@�u@@�u@@�u@@1'@?�@?�;@?��@?��@?��@?\)@?;d@?;d@?+@>��@>�@>��@>@=�T@=��@=@=�h@=O�@=/@=V@<�@<�D@;��@;dZ@;"�@;@;o@:�@:��@:~�@:J@9X@9�@8�`@8Ĝ@8��@8�@8Q�@7�@7+@6��@6@5@5�h@5?}@4�j@4�@3��@2�@2��@2n�@2M�@2=q@1��@1hs@1%@0Ĝ@0bN@0A�@0 �@/�w@.�y@.E�@-�@-��@-��@-�h@-�@-`B@-?}@-�@,�j@,I�@,(�@+S�@*M�@)�#@)��@)7L@)&�@)�@)�@(��@(Ĝ@(Ĝ@(�@(A�@(  @'�w@&ȴ@&5?@%@%��@%p�@%p�@%`B@%`B@%?}@%?}@%?}@%?}@%/@%V@$��@$�j@$�D@$z�@$9X@#��@#t�@#C�@#C�@#C�@#dZ@#�@#�@#�@#��@#t�@#S�@#33@#"�@#@"�H@"�H@"��@"��@"^5@"=q@"�@"J@!�^@ Ĝ@ bN@�;@��@��@�P@|�@|�@l�@l�@l�@|�@|�@l�@l�@l�@;d@
=@�@ȴ@�R@ff@�@��@�D@9X@(�@�@�@��@�
@ƨ@ƨ@ƨ@ƨ@��@��@t�@C�@o@��@�\@~�@n�@n�@=q@-@J@�@�#@�^@��@�7@x�@hs@X@%@�`@1'@|�@�@�R@v�@E�@{@��@�T@��@�h@�@V@�@z�@Z@Z@9X@1@��@�
@ƨ@ƨ@�F@��@t�@dZ@S�@S�@33@"�@�H@~�@=q@�@��@��@�7@�7@hs@��@��@�u@r�@r�@Q�@A�@b@��@��@�P@|�@l�@+@+@�@��@�@�R@��@��@V@@@�h@`B@�@��@�/@z�@1@ƨ@t�@dZ@33@o@
�@
��@
n�@
^5@
M�@
-@
J@	��@	��@	�7@	x�@	x�@	X@	X@	&�@	%@��@�@Q�@1'@��@l�@\)@K�@;d@�+@$�@��@�-@��@��@�h@�@`B@O�@/@�/@��@��@�j@�@��@�D@��@��@��@��@��@�D@Z@(�@�
@�F@��@dZ@C�@33@33@33@33@33@33@"�@o@@�@�H@��@��@��@��@=q@��@�^@X@7L@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʡ�Aʣ�Aʣ�Aʥ�Aʥ�Aʧ�Aʣ�Aʣ�Aʥ�Aʧ�Aʥ�Aʧ�Aʥ�Aʣ�Aʡ�Aʡ�Aʟ�Aʡ�Aʣ�Aʣ�Aʟ�Aʥ�Aʥ�Aʟ�Aʛ�AʋDA�\)A�{A��HA�hsA���Aȏ\A��TA���A�JA�%A��DA��`A� �A���A��mA�%A���A�(�A���A�33A��A�~�A��wA�v�A�~�A��9A�A�jA��A��A���A�O�A�A�A��A�ZA�oA���A���A�=qA���A�/A�n�A�Q�A��-A�z�A���A�33A�G�A��hA�VA�
=A��A�|�A�n�A�jA�`BA�O�A�oA��mA�A�|�A��9A�%A���A� �A�p�A���A��A��yA���A�jA�v�A���A��TA�5?A�oA��At�A~9XA|^5A{�
A{�A{K�Az�`Az(�Awx�Au\)As�Ap�!Ao��An�DAnA�Am��Am�-Amp�Am7LAl�AkG�AjffAiXAhr�Ag"�Ae�AcS�AbbAa�;Aa��A`��A_�FA^1A]hsA\�uAZ�AZbAX�AXQ�AW�AV�AUx�AT{AS?}AS�AR��AR(�AQ`BAPz�AN~�ANAM33AK��AI�
AIoAG�TAF�`AEx�AD�AC��AC
=AAXA@�9A@A�A>��A=x�A<$�A;�A;S�A9hsA8��A85?A7�FA7XA733A6��A5�A4��A4M�A3�mA3�^A2ȴA1��A01A.=qA.�A.bA.1A-��A,��A,{A+&�A)�wA(bA&n�A%�;A%��A%�A%;dA%�A%%A$��A$�yA$�HA$�!A$ffA#;dA"�/A"��A"��A!l�A ��A�wAG�A��AC�A��AffA��AS�A��AbNA�A��A33A^5A�^AM�AdZAz�A  A��A�\A��A
bA�+AƨA�A?}A�A��A�7A��A�PA"�A�^A�Av�At�A ��@��@��T@�I�@��@�dZ@�5?@�Ĝ@��@�~�@���@���@�t�@���@�&�@�Ĝ@�\)@�u@�^5@�
=@�u@�;d@���@��@��D@�  @��m@��@�E�@�J@�&�@���@ԣ�@�r�@�1'@��@���@ӥ�@�C�@���@�ff@�@�7L@��`@�\)@��#@�/@��`@�1'@��;@˕�@ʸR@���@�$�@š�@�&�@���@ă@�Q�@� �@å�@�S�@��y@�E�@�?}@�r�@���@��@���@��T@���@��@�dZ@��@���@��T@���@�(�@�ƨ@�"�@���@��^@��9@� �@�"�@�-@���@�b@��
@��P@��R@��@�7L@�A�@�;d@�V@���@�~�@�5?@���@�1@��H@�n�@�E�@�{@��7@���@�Q�@��@��+@��@�@�?}@�V@��j@�A�@�K�@��@��+@�^5@�=q@�$�@�{@���@���@�X@� �@��
@��F@��@�l�@��@���@��@���@�v�@�E�@�$�@�@�@���@�G�@���@��j@���@�o@�5?@���@��h@�`B@�/@�/@�r�@���@�"�@���@�n�@�M�@�5?@�$�@���@���@���@��7@�p�@�`B@�G�@���@�j@��m@��@�+@�@�ȴ@���@�~�@�$�@��@���@�{@�@�@��7@�`B@��@��@��u@�(�@��m@��@�l�@�33@��y@�n�@�M�@�-@�{@���@��@��#@��#@���@��^@�p�@��/@��@��u@�I�@�1'@�(�@�(�@�(�@�(�@� �@��@��@�b@�  @\)@}�h@|�D@|z�@|I�@{�m@{ƨ@{ƨ@{ƨ@{��@{t�@{S�@{C�@{"�@z�@z��@z~�@z�@y��@y��@y��@y�7@y�@x�9@xr�@xQ�@xb@w�P@wK�@v�+@v@u�T@u@u��@u�@up�@up�@t��@t�@s�
@s��@sS�@r��@qx�@pĜ@pr�@p  @o�@n�R@m��@l��@l��@lz�@kdZ@j��@j^5@j�@i��@i�#@i�#@i��@i��@i�^@i�^@ix�@hQ�@g��@gl�@f�y@f��@fȴ@f��@f��@fȴ@f�y@f�y@f�R@f$�@e�h@e�@dI�@c�
@c�@b��@b-@bJ@a�^@a��@a�7@aG�@a&�@`��@`Ĝ@`�u@`A�@`b@`b@`b@`  @_�w@_K�@^ff@^@]�T@]��@\�@\j@\1@[33@Y�^@YX@X�`@XQ�@W
=@VE�@U�h@T9X@SdZ@R�@R�!@R��@R�!@R~�@R-@Q��@Q��@Qhs@Q7L@PĜ@Pr�@P1'@P  @O�;@O�@O�P@O\)@OK�@O;d@O+@O
=@N��@N�y@N��@Nff@M�@M�@L�D@Lz�@LZ@K��@K��@KC�@J�H@I�@Ihs@HĜ@H�u@H�@HA�@H �@G�;@Gl�@F�y@Fv�@F{@E��@E�h@Ep�@E/@EV@D�/@D�@D�@C�m@C�F@C�@C@A��@Ahs@@��@@��@@�u@@�u@@�u@@1'@?�@?�;@?��@?��@?��@?\)@?;d@?;d@?+@>��@>�@>��@>@=�T@=��@=@=�h@=O�@=/@=V@<�@<�D@;��@;dZ@;"�@;@;o@:�@:��@:~�@:J@9X@9�@8�`@8Ĝ@8��@8�@8Q�@7�@7+@6��@6@5@5�h@5?}@4�j@4�@3��@2�@2��@2n�@2M�@2=q@1��@1hs@1%@0Ĝ@0bN@0A�@0 �@/�w@.�y@.E�@-�@-��@-��@-�h@-�@-`B@-?}@-�@,�j@,I�@,(�@+S�@*M�@)�#@)��@)7L@)&�@)�@)�@(��@(Ĝ@(Ĝ@(�@(A�@(  @'�w@&ȴ@&5?@%@%��@%p�@%p�@%`B@%`B@%?}@%?}@%?}@%?}@%/@%V@$��@$�j@$�D@$z�@$9X@#��@#t�@#C�@#C�@#C�@#dZ@#�@#�@#�@#��@#t�@#S�@#33@#"�@#@"�H@"�H@"��@"��@"^5@"=q@"�@"J@!�^@ Ĝ@ bN@�;@��@��@�P@|�@|�@l�@l�@l�@|�@|�@l�@l�@l�@;d@
=@�@ȴ@�R@ff@�@��@�D@9X@(�@�@�@��@�
@ƨ@ƨ@ƨ@ƨ@��@��@t�@C�@o@��@�\@~�@n�@n�@=q@-@J@�@�#@�^@��@�7@x�@hs@X@%@�`@1'@|�@�@�R@v�@E�@{@��@�T@��@�h@�@V@�@z�@Z@Z@9X@1@��@�
@ƨ@ƨ@�F@��@t�@dZ@S�@S�@33@"�@�H@~�@=q@�@��@��@�7@�7@hs@��@��@�u@r�@r�@Q�@A�@b@��@��@�P@|�@l�@+@+@�@��@�@�R@��@��@V@@@�h@`B@�@��@�/@z�@1@ƨ@t�@dZ@33@o@
�@
��@
n�@
^5@
M�@
-@
J@	��@	��@	�7@	x�@	x�@	X@	X@	&�@	%@��@�@Q�@1'@��@l�@\)@K�@;d@�+@$�@��@�-@��@��@�h@�@`B@O�@/@�/@��@��@�j@�@��@�D@��@��@��@��@��@�D@Z@(�@�
@�F@��@dZ@C�@33@33@33@33@33@33@"�@o@@�@�H@��@��@��@��@=q@��@�^@X@7L@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9XB9XB9XB9XB9XB8RB9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B<jB=qB;dB<jB?}BB�BD�BC�BA�B@�BC�B?}B<jB9XB0!BDB�B�\B��B�{B��BdZBL�Bv�B_;BW
BhsBk�B`BBI�BW
BF�B<jBA�B&�B6FB33B(�B{B�B�B�fB��B��B�BɺB�B�jB�FB��B�B�VB��B�7B|�Bp�BcTB]/BE�BhsB`BBgmBhsBgmBdZB_;B\)BXBN�B9XB0!B)�B�B
�B
�B
�B
��B
��B
�3B
��B
��B
� B
x�B
ffB
iyB
[#B
Q�B
M�B
VB
XB
R�B
G�B
6FB
oB
	7B
B	�HB
B	��B
+B
	7B
%B
B	��B	�B	�;B	�#B	��B	��B	��B	�'B	��B	�B	�qB	�FB	��B	��B	�PB	��B	�VB	~�B	�B	� B	�%B	{�B	u�B	dZB	ffB	gmB	m�B	e`B	]/B	Q�B	G�B	5?B	K�B	I�B	<jB	.B	7LB	-B	+B	�B	�B	{B	oB	%B	JB	VB��B��B�B��B�B�/B�NB�TB�HB�HB�BB�B��B��B��BŢBB�?B��B��B��B�3B�9B�!B��B��B��B�bB�B|�B�B�hB��B��B��B��B��B��B��B��B�{B�hB�B�PB�VB�+Bt�Bs�BiyBO�BH�BP�BbNBl�BffBbNBaHBbNB_;B\)BZBN�BO�BF�BG�BJ�BK�BI�B7LB(�B{B%�B9XBA�B?}B9XB.B?}BC�B=qB0!B�B%�B(�B!�B"�B�B�B"�B1'B-B$�B �B(�B �B'�B%�B�B�B%�B%�B�B
=BVB	7BoB�B#�B �B�B"�B �B\B��B��B$�B-B1'B33B33B49B5?B33B2-B2-B2-B0!B/B/B$�B&�B33B8RB5?B7LB5?B,B#�B�B=qB>wB@�BA�BA�B@�B=qB>wB<jB9XB49B9XB<jB@�BB�B:^B:^BE�BE�BH�BG�BB�B@�BC�BC�B:^B)�B.BJ�BS�BQ�BYBXBe`BjBjBgmBbNBm�BffBdZB_;BgmBq�B{�Bz�Bs�B�B�DB�PB�JB�1B�1B�+B�7B�1B��B��B��B��B��B��B��B�B�B�-B�-B�9B�9B�3B�'B�B��B�dB�wB�}B�wB�}BBÖBÖBŢBŢBƨBƨBƨBǮBǮBȴB��B��B��B��B�)B�TB�TB�ZB�`B�BB�`B�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	%B	
=B	hB	oB	�B	�B	�B	!�B	'�B	'�B	'�B	'�B	,B	.B	/B	6FB	33B	7LB	;dB	=qB	?}B	B�B	F�B	J�B	S�B	VB	YB	ZB	[#B	\)B	\)B	\)B	[#B	ZB	ZB	dZB	gmB	iyB	l�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	m�B	l�B	iyB	iyB	v�B	�B	�B	�B	�B	�+B	�+B	�%B	�+B	�1B	�7B	�=B	�DB	�JB	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�3B	�3B	�3B	�'B	�?B	�XB	��B	�wB	�^B	�}B	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ȴB	ǮB	ŢB	��B	ŢB	ȴB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	��B	�B	�B	�B	�#B	�/B	�#B	�BB	�NB	�NB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�B	�yB	�sB	�mB	�fB	�fB	�B	�B	�B	�sB	�yB	�yB	�sB	�`B	�B	�B	�B	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	7B
1B
+B
+B
+B
+B
B
	7B
DB
\B
bB
VB
\B
VB
VB
VB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
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
 �B
 �B
!�B
!�B
 �B
�B
�B
#�B
#�B
%�B
%�B
$�B
#�B
"�B
"�B
!�B
$�B
%�B
&�B
%�B
$�B
$�B
"�B
!�B
"�B
$�B
&�B
&�B
%�B
$�B
#�B
%�B
%�B
)�B
+B
+B
+B
)�B
(�B
+B
.B
-B
/B
.B
,B
+B
.B
1'B
33B
33B
49B
49B
33B
33B
2-B
1'B
0!B
2-B
/B
/B
6FB
7LB
7LB
9XB
:^B
9XB
9XB
8RB
9XB
8RB
7LB
7LB
7LB
49B
7LB
;dB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
A�B
C�B
D�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
B�B
>wB
C�B
E�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
I�B
I�B
I�B
G�B
E�B
I�B
L�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
R�B
Q�B
Q�B
P�B
Q�B
R�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
S�B
T�B
Q�B
R�B
VB
W
B
YB
ZB
[#B
[#B
\)B
\)B
[#B
[#B
ZB
ZB
]/B
^5B
^5B
^5B
^5B
_;B
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
_;B
^5B
_;B
`BB
_;B
aHB
aHB
aHB
`BB
_;B
aHB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
dZB
dZB
cTB
e`B
dZB
dZB
dZB
e`B
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
gmB
hsB
iyB
iyB
iyB
jB
iyB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
k�B
l�B
m�B
k�B
l�B
o�B
o�B
m�B
k�B
n�B
o�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
q�B
s�B
t�B
s�B
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
r�B
r�B
t�B
t�B
t�B
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
v�B
u�B
v�B
u�B
t�B
s�B
u�B
u�B
u�B
w�B
x�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9XB9XB9XB9XB9XB8RB9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B<jB=qB;dB<jB?}BB�BD�BC�BBBA BDB@�B=�B:�B3B�B�B�B��B��B��Bi�BR�Bx�Bc�BZ�Bk6Bm]BcBMBX�BIlB>�BC{B*eB7�B4TB*0BSB�`B�5B�*BοB�uBچB��B��B��B��B�,B�YB��B�IB��B~�Br�BezB_�BH1BhsBa-Bg�Bh�Bg�Bd�B_�B\�BX�BO�B;JB1�B+6BB
�B
��B
�kB
̈́B
�'B
�TB
��B
�$B
��B
z�B
h�B
j�B
]IB
S�B
O�B
V�B
XyB
S[B
H�B
7�B
�B
�B
B	�FB
�B	�B
zB
	�B
�B
{B	�wB	�B	��B	�]B	�SB	�B	�AB	�MB	�RB	�cB	��B	��B	��B	�QB	�BB	�SB	��B	��B	�?B	�oB	��B	}B	v�B	f2B	g�B	hXB	m�B	fB	^B	S&B	IB	7fB	L~B	J�B	>BB	0�B	8lB	.�B	,WB	!|B	�B	B	uB	1B	PB	BB��B��B�-B��B��B�pB� B�&B��B��B�B��B�B�-B�B�?B�GB��B��B��B��B�hB�TB�UB��B�B��B��B�B.B��B�B��B��B�	B��B��B��B��B��B��B� B��B��B��B��BvzBu%BkkBS@BK�BR�Bc Bl�BgBc:BbBb�B_�B\�BZ�BPBQ BH�BIBK�BL�BJ�B9>B+kB�B'�B:*BBB@ B:*B/�B?�BC�B=�B1B �B&�B)�B#B#�B �BB#�B1vB-�B%�B!�B)�B!�B(sB&�B �B �B&�B&fB�BB�BxB�B�B$ZB!�B �B#nB!bB B��B�B%FB-]B1[B3hB3�B4nB5tB3�B2�B2�B2�B0�B/�B/�B&B'�B3�B8�B5�B7�B5�B,�B%FB;B=�B>�B@�BA�BA�B@�B=�B>�B<�B9�B5%B9�B=B@�BB�B;JB;JBF%BF%BIBH1BCGBAUBDBC�B;JB+�B0BK^BT{BR�BY�BYBe�Bj�Bj�Bh$BcnBm�BgRBe`B`�BhsBraB|PB{Bu%B��B��B��B��B��B��B��B��B�B��B�B�:B�$B�DB�mB��B�QB�cB�GB�aB�TB�nB�hB�vB��B��B�B��B��B��B��BªB��B��B��B��B��B��B��B��B��B�B�B�B��B�}BܒB�nB�B�B�B��B��B�B�B��B��B��B��B�B�0B�B�B�(B�B�.B�PB�jB	 iB	tB	
�B	�B	�B	�B	�B	B	!�B	'�B	'�B	(
B	(>B	,=B	.IB	/OB	6FB	3�B	7�B	;�B	=�B	?�B	B�B	F�B	KB	TB	VB	Y1B	Z7B	[=B	\CB	\CB	\CB	[qB	ZkB	Z�B	dtB	g�B	i�B	l�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	m�B	l�B	i�B	j0B	w2B	�B	�'B	�GB	�9B	�+B	�+B	�?B	�EB	�KB	�RB	�XB	�^B	�dB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�4B	�B	�B	�B	�B	�B	�B	�B	�DB	�6B	�5B	�5B	�OB	�]B	��B	�iB	�MB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�9B	�2B	�9B	�eB	�_B	�WB	�dB	�qB	�\B	�hB	�B	�tB	�tB	�tB	�zB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�B	�B	��B	��B	�B	��B	�B	�(B	�B	�B	�(B	�B
;B
'B
'B
GB
GB
-B
B
3B
3B
B
3B
3B
-B
-B
AB
[B
MB
	7B
KB
_B
_B
_B
_B
�B
	lB
xB
vB
bB
pB
vB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
 �B
�B
/B
$B
#�B
%�B
%�B
$�B
#�B
#B
#B
"B
%B
%�B
'B
%�B
$�B
%B
#B
"4B
#B
%B
'B
'B
&2B
%B
$B
&B
&LB
*B
+B
+B
+B
*0B
)*B
+6B
./B
-]B
/5B
./B
,=B
+QB
.IB
1AB
3MB
3MB
4TB
49B
3hB
3MB
2GB
1vB
0UB
2GB
/�B
/�B
6`B
7fB
7fB
9XB
:xB
9XB
9rB
8lB
9XB
8lB
7fB
7fB
7fB
4�B
7�B
;B
=�B
=�B
?}B
?�B
?}B
?�B
?}B
?�B
?}B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
A�B
C�B
D�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
B�B
>�B
C�B
E�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
I�B
I�B
I�B
G�B
E�B
I�B
MB
OB
Q B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
R�B
RB
R�B
RB
RB
QB
RB
S&B
T�B
UB
T�B
TB
T�B
UB
U2B
VB
VB
VB
VB
VB
VB
VB
T,B
UB
R:B
S@B
VB
W?B
YKB
Z7B
[=B
[=B
\)B
\CB
[=B
[=B
ZkB
Z7B
]IB
^5B
^5B
^OB
^OB
_VB
^5B
_;B
_;B
_;B
_pB
_VB
_;B
`\B
`BB
_VB
_VB
_VB
^jB
_VB
`\B
_VB
abB
abB
aHB
`\B
_pB
abB
bhB
cTB
cTB
cnB
cTB
bhB
bhB
c�B
cnB
dZB
dtB
cnB
e`B
dtB
dtB
d�B
ezB
e`B
dtB
dtB
dtB
ezB
ezB
ezB
ezB
f�B
f�B
e�B
e�B
g�B
h�B
iyB
i�B
i�B
j�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
k�B
l�B
m�B
k�B
l�B
o�B
o�B
m�B
k�B
n�B
o�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
q�B
s�B
t�B
s�B
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
r�B
r�B
t�B
t�B
t�B
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
v�B
u�B
v�B
u�B
t�B
tB
u�B
u�B
u�B
w�B
x�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811200036492018112000364920181120003649201811200200162018112002001620181120020016201811210030042018112100300420181121003004  JA  ARFMdecpA19c                                                                20181116093621  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181116003623  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181116003626  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181116003627  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181116003627  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181116003627  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181116003628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181116003628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181116003628  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181116003628                      G�O�G�O�G�O�                JA  ARUP                                                                        20181116005610                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181116153406  CV  JULD            G�O�G�O�Fć�                JM  ARCAJMQC2.0                                                                 20181119153649  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181119153649  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181119170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181120153004  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                