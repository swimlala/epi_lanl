CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-18T00:35:14Z creation;2018-03-18T00:35:18Z conversion to V3.1;2019-12-19T07:46:55Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180318003514  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_220                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�T5�I�1   @�T6q��@:'�@���dg�{J#:1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B(Q�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�HCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�DD~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D1D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�<)D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݼ)D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�B�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��jA��jA��jA��jA��jA��jA��^A��jA��RA���A�A�A��^A��jA��jA��jA��RA��9A��!A���A���A���A��7A��A�p�A�hsA�`BA�K�A�&�A��`A���A���A��wA�v�A�33A���A���A�33A���A���A�M�A��mA��mA�jA��A�=qA���A�ƨA�/A��mA��jA�9XA�1A� �A��PA���A��A�jA�{A���A���A�9XA�hsA���A��A���A��A�33A�M�A�  A�z�A��A�ȴA�JA���A�p�A��A�l�A�bA��AVA~5?A}|�A|�A|1Ay�FAvZAvbAu�At�jAtr�Ar��Ao�^Am`BAl�/AlbNAl�Ak7LAj^5Ai�FAi%Ah��Ag�Ae�-AdAc;dAb�+AaA`�A_A_;dA^JA]�wA];dA[�AZ��AZ�AY�#AXbAU�-ASt�AR��ARz�ARbAQ��AQhsAP��AP�9AP�+AP1'AOAN9XAM��AM
=AK�-AK
=AJr�AI�PAI&�AHI�AGdZAFA�AE��AE�wAE�AC��AB~�AAS�A@�yA@A�A?��A>�!A> �A<�/A< �A;%A:�9A9�-A9
=A8�A8�A8�A7VA5VA4ĜA4=qA3�7A2�A2(�A1C�A0�A/�A-��A-��A,�A,r�A,�A+��A*��A*�A*M�A)�A)�A)C�A(��A'+A%�#A$��A$Q�A#/A"9XA ��A E�A��A��A�PAl�A/A�A5?A�HA{A�-A��A��An�A{A�wA�\A�HA�A�A�+A{Ap�A�+A��A%A��AdZA
JA	7LAv�A�wA��A~�A{A�;A�^A�hA\)A�AĜA^5A�TA�A��A�wA ��@��@�ff@�/@�ƨ@�?}@�dZ@���@�`B@��@�$�@�7L@��@�K�@�\@�p�@�j@�33@�=q@���@��H@���@��@�R@�x�@��u@�j@�Z@��@���@��
@ߥ�@�o@�7L@�S�@�@ڇ+@�M�@��@ى7@���@؛�@���@��H@�-@�t�@�5?@щ7@�z�@��
@�+@�M�@���@�@ʇ+@�&�@Ɵ�@��@ļj@��@�  @��m@��m@���@å�@���@�~�@�$�@���@�|�@���@�`B@�/@���@�|�@�$�@�&�@�1'@�33@��!@���@��@�;d@�M�@��h@���@���@�
=@�V@��@�p�@���@�+@�hs@��@�1'@��w@�C�@���@�=q@�hs@���@�9X@��;@�K�@��y@���@�V@�@�/@��j@�z�@�A�@��m@��F@��@�+@�V@��@�/@���@���@���@�33@���@��+@�^5@�v�@���@���@�I�@��F@�@�v�@�M�@�$�@��@��^@���@�x�@�G�@���@��u@�+@�ff@�{@��^@�p�@�V@��u@�bN@�bN@�Z@�A�@���@��@��H@��+@�~�@�^5@�$�@��@�@���@���@�X@��/@��9@�j@�9X@��@��!@�E�@��-@���@���@�hs@�?}@�%@�Z@��P@�;d@���@�{@�X@�7L@�O�@�7L@��j@��@�Z@�bN@�1@��w@��F@�t�@��@�v�@�$�@��h@�x�@�`B@�G�@�V@��`@��/@��j@�z�@�1'@��@�@�@~��@~V@}��@}�@|�j@{ƨ@{dZ@{o@z��@z��@z�!@z�!@z�!@z��@z�@y��@y%@x��@x��@x�9@xA�@w��@w�P@wl�@v��@v��@v�+@v{@u�@t�j@tI�@s��@s�F@st�@sdZ@sS�@s"�@r�H@r�!@r~�@rn�@rM�@rJ@q��@p�`@pĜ@p�9@pr�@p �@pA�@pr�@p�u@p�u@pbN@o��@n�@n��@nff@n@m��@m�@mp�@m�@l��@k�
@k��@kt�@k��@k��@k��@k��@k�@ko@j�@j��@j^5@jM�@j=q@jJ@i��@i��@i7L@i%@hĜ@h�9@h��@h��@h�u@hbN@g��@g|�@g�@f�y@fȴ@f��@f��@fv�@f$�@e�T@eV@d�@d�/@d��@d�j@d�@d�D@dz�@dI�@c�m@c��@ct�@c@b~�@a��@aX@aG�@a�@` �@_�w@_�@_�P@_l�@_;d@^��@]�@]��@]`B@]�@\��@\�@[ƨ@[��@[dZ@[@Z��@Z^5@Z�@Y��@Y&�@X�@X �@W��@W;d@V�@V�R@V�+@V5?@U��@U�h@UV@T9X@T�@S�
@S��@SdZ@R�@R�!@R��@Q�@Q��@Qhs@QX@QG�@Q7L@Q%@PbN@O;d@N��@N5?@M��@M?}@L�j@L�D@L(�@K��@Ko@JM�@J-@Ihs@IG�@I&�@H�9@Hr�@G�@G�@G�P@Gl�@Gl�@G\)@G
=@F$�@F{@F@F@E�@E�-@E`B@D�@D��@DI�@DI�@D(�@C�m@C�m@Cƨ@C�F@CdZ@Co@B�@B�H@B��@B��@B�!@B=q@A��@A�#@A�#@A��@@�u@@b@?�w@?l�@?l�@>�y@>�@>��@>�+@>ff@>E�@>$�@=��@=O�@=V@<�D@<j@<9X@;��@;�m@;ƨ@;S�@:�\@:=q@9��@97L@8�`@8��@8Ĝ@8�9@8�u@8bN@8  @7;d@6��@6�@6�R@6v�@65?@6{@5@5`B@5�@4�@4�/@4z�@4Z@4I�@41@3��@3dZ@333@3@2��@2^5@2-@1�#@1��@1��@1��@1��@1�7@1hs@1&�@0��@0�u@0bN@/�@/��@/|�@/;d@.ȴ@.��@.v�@.5?@-�T@-�-@-p�@,��@,��@,I�@+�m@+��@+dZ@+dZ@+C�@+"�@+"�@*�@*�!@*��@*�\@*~�@*^5@*�@)��@)�7@)7L@(�@(b@'�w@'�P@'l�@';d@'
=@&ȴ@&�R@&��@&E�@%�T@%��@%�@%`B@%?}@%?}@%?}@$��@$��@$��@$�D@$�D@$z�@$1@#dZ@#o@"�@"M�@!��@!7L@ ��@ Ĝ@�@�w@\)@�+@{@@@�@�h@�@�@�@p�@p�@`B@O�@O�@?}@?}@/@�@V@��@�/@�j@��@(�@S�@33@"�@"�@"�@��@��@�@��@�^@hs@7L@Ĝ@bN@Q�@ �@b@�@|�@�@�R@��@�+@v�@ff@V@E�@5?@$�@�-@�@/@��@�/@�@j@Z@I�@I�@1@��@t�@t�@dZ@S�@C�@"�@"�@o@@�@�@�H@��@�!@n�@=q@��@��@X@&�@&�@&�@�9@r�@1'@��@\)@��@�@�R@��@�+@ff@E�@5?@�T@��@�h@�h@�h@�h@�@?}@��@�j@�@��@�D@Z@9X@(�@��@ƨ@�@dZ@S�@"�@o@@
��@
�!@
�\@
n�@
M�@
=q@
=q@
-@
-@	�@	�7@	X@	G�@	&�@��@��@�`@�`@��@��@Ĝ@Ĝ@Ĝ@�9@bN@b@��@��@|�@K�@+@��@�@ȴ@�R@��@�+@v�@ff@V@V@V@V@V@E�@$�@{@@�T@��@�h@`B@V@�@�/@��@��@j@9X@(�@(�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��jA��jA��jA��jA��jA��jA��^A��jA��RA���A�A�A��^A��jA��jA��jA��RA��9A��!A���A���A���A��7A��A�p�A�hsA�`BA�K�A�&�A��`A���A���A��wA�v�A�33A���A���A�33A���A���A�M�A��mA��mA�jA��A�=qA���A�ƨA�/A��mA��jA�9XA�1A� �A��PA���A��A�jA�{A���A���A�9XA�hsA���A��A���A��A�33A�M�A�  A�z�A��A�ȴA�JA���A�p�A��A�l�A�bA��AVA~5?A}|�A|�A|1Ay�FAvZAvbAu�At�jAtr�Ar��Ao�^Am`BAl�/AlbNAl�Ak7LAj^5Ai�FAi%Ah��Ag�Ae�-AdAc;dAb�+AaA`�A_A_;dA^JA]�wA];dA[�AZ��AZ�AY�#AXbAU�-ASt�AR��ARz�ARbAQ��AQhsAP��AP�9AP�+AP1'AOAN9XAM��AM
=AK�-AK
=AJr�AI�PAI&�AHI�AGdZAFA�AE��AE�wAE�AC��AB~�AAS�A@�yA@A�A?��A>�!A> �A<�/A< �A;%A:�9A9�-A9
=A8�A8�A8�A7VA5VA4ĜA4=qA3�7A2�A2(�A1C�A0�A/�A-��A-��A,�A,r�A,�A+��A*��A*�A*M�A)�A)�A)C�A(��A'+A%�#A$��A$Q�A#/A"9XA ��A E�A��A��A�PAl�A/A�A5?A�HA{A�-A��A��An�A{A�wA�\A�HA�A�A�+A{Ap�A�+A��A%A��AdZA
JA	7LAv�A�wA��A~�A{A�;A�^A�hA\)A�AĜA^5A�TA�A��A�wA ��@��@�ff@�/@�ƨ@�?}@�dZ@���@�`B@��@�$�@�7L@��@�K�@�\@�p�@�j@�33@�=q@���@��H@���@��@�R@�x�@��u@�j@�Z@��@���@��
@ߥ�@�o@�7L@�S�@�@ڇ+@�M�@��@ى7@���@؛�@���@��H@�-@�t�@�5?@щ7@�z�@��
@�+@�M�@���@�@ʇ+@�&�@Ɵ�@��@ļj@��@�  @��m@��m@���@å�@���@�~�@�$�@���@�|�@���@�`B@�/@���@�|�@�$�@�&�@�1'@�33@��!@���@��@�;d@�M�@��h@���@���@�
=@�V@��@�p�@���@�+@�hs@��@�1'@��w@�C�@���@�=q@�hs@���@�9X@��;@�K�@��y@���@�V@�@�/@��j@�z�@�A�@��m@��F@��@�+@�V@��@�/@���@���@���@�33@���@��+@�^5@�v�@���@���@�I�@��F@�@�v�@�M�@�$�@��@��^@���@�x�@�G�@���@��u@�+@�ff@�{@��^@�p�@�V@��u@�bN@�bN@�Z@�A�@���@��@��H@��+@�~�@�^5@�$�@��@�@���@���@�X@��/@��9@�j@�9X@��@��!@�E�@��-@���@���@�hs@�?}@�%@�Z@��P@�;d@���@�{@�X@�7L@�O�@�7L@��j@��@�Z@�bN@�1@��w@��F@�t�@��@�v�@�$�@��h@�x�@�`B@�G�@�V@��`@��/@��j@�z�@�1'@��@�@�@~��@~V@}��@}�@|�j@{ƨ@{dZ@{o@z��@z��@z�!@z�!@z�!@z��@z�@y��@y%@x��@x��@x�9@xA�@w��@w�P@wl�@v��@v��@v�+@v{@u�@t�j@tI�@s��@s�F@st�@sdZ@sS�@s"�@r�H@r�!@r~�@rn�@rM�@rJ@q��@p�`@pĜ@p�9@pr�@p �@pA�@pr�@p�u@p�u@pbN@o��@n�@n��@nff@n@m��@m�@mp�@m�@l��@k�
@k��@kt�@k��@k��@k��@k��@k�@ko@j�@j��@j^5@jM�@j=q@jJ@i��@i��@i7L@i%@hĜ@h�9@h��@h��@h�u@hbN@g��@g|�@g�@f�y@fȴ@f��@f��@fv�@f$�@e�T@eV@d�@d�/@d��@d�j@d�@d�D@dz�@dI�@c�m@c��@ct�@c@b~�@a��@aX@aG�@a�@` �@_�w@_�@_�P@_l�@_;d@^��@]�@]��@]`B@]�@\��@\�@[ƨ@[��@[dZ@[@Z��@Z^5@Z�@Y��@Y&�@X�@X �@W��@W;d@V�@V�R@V�+@V5?@U��@U�h@UV@T9X@T�@S�
@S��@SdZ@R�@R�!@R��@Q�@Q��@Qhs@QX@QG�@Q7L@Q%@PbN@O;d@N��@N5?@M��@M?}@L�j@L�D@L(�@K��@Ko@JM�@J-@Ihs@IG�@I&�@H�9@Hr�@G�@G�@G�P@Gl�@Gl�@G\)@G
=@F$�@F{@F@F@E�@E�-@E`B@D�@D��@DI�@DI�@D(�@C�m@C�m@Cƨ@C�F@CdZ@Co@B�@B�H@B��@B��@B�!@B=q@A��@A�#@A�#@A��@@�u@@b@?�w@?l�@?l�@>�y@>�@>��@>�+@>ff@>E�@>$�@=��@=O�@=V@<�D@<j@<9X@;��@;�m@;ƨ@;S�@:�\@:=q@9��@97L@8�`@8��@8Ĝ@8�9@8�u@8bN@8  @7;d@6��@6�@6�R@6v�@65?@6{@5@5`B@5�@4�@4�/@4z�@4Z@4I�@41@3��@3dZ@333@3@2��@2^5@2-@1�#@1��@1��@1��@1��@1�7@1hs@1&�@0��@0�u@0bN@/�@/��@/|�@/;d@.ȴ@.��@.v�@.5?@-�T@-�-@-p�@,��@,��@,I�@+�m@+��@+dZ@+dZ@+C�@+"�@+"�@*�@*�!@*��@*�\@*~�@*^5@*�@)��@)�7@)7L@(�@(b@'�w@'�P@'l�@';d@'
=@&ȴ@&�R@&��@&E�@%�T@%��@%�@%`B@%?}@%?}@%?}@$��@$��@$��@$�D@$�D@$z�@$1@#dZ@#o@"�@"M�@!��@!7L@ ��@ Ĝ@�@�w@\)@�+@{@@@�@�h@�@�@�@p�@p�@`B@O�@O�@?}@?}@/@�@V@��@�/@�j@��@(�@S�@33@"�@"�@"�@��@��@�@��@�^@hs@7L@Ĝ@bN@Q�@ �@b@�@|�@�@�R@��@�+@v�@ff@V@E�@5?@$�@�-@�@/@��@�/@�@j@Z@I�@I�@1@��@t�@t�@dZ@S�@C�@"�@"�@o@@�@�@�H@��@�!@n�@=q@��@��@X@&�@&�@&�@�9@r�@1'@��@\)@��@�@�R@��@�+@ff@E�@5?@�T@��@�h@�h@�h@�h@�@?}@��@�j@�@��@�D@Z@9X@(�@��@ƨ@�@dZ@S�@"�@o@@
��@
�!@
�\@
n�@
M�@
=q@
=q@
-@
-@	�@	�7@	X@	G�@	&�@��@��@�`@�`@��@��@Ĝ@Ĝ@Ĝ@�9@bN@b@��@��@|�@K�@+@��@�@ȴ@�R@��@�+@v�@ff@V@V@V@V@V@E�@$�@{@@�T@��@�h@`B@V@�@�/@��@��@j@9X@(�@(�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B{�B�%B�1B� BffBt�Bo�BffBS�BW
BD�B�B  B��B�B�B�B�/B�wB��B�=B��B��B��B��B�\B�7B|�BffB\)BD�B8RB.BoB
�)B
�5B
�fB
�ZB
�#B
��B
�!B
�B
��B
��B
�B
cTB
r�B
k�B
ffB
_;B
Q�B
7LB
�B
2-B
-B
#�B
�B

=B	�`B	�;B	��B	��B	�B	�sB	�5B	�HB	�
B	��B	ƨB	�'B	�'B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	�JB	�+B	�DB	}�B	dZB	_;B	P�B	cTB	bNB	^5B	_;B	\)B	YB	]/B	aHB	\)B	P�B	J�B	E�B	@�B	9XB	<jB	;dB	1'B	33B	)�B	 �B	 �B	 �B	"�B	�B	B		7B	B	+B	B��B�B��B�sB�yB�NB�B�`B�sB�B�B�ZB�BĜB��B��BɺBƨB��B�XB�jB�-B��B�?B�-B�!B�'B�B��B�B�B��B��B��B��B�1B�B�+B�JB�B|�B~�B� B�1B�=B�+B�+B�B}�Bt�BjBjBo�B`BBZBW
B_;B_;BM�BF�BP�BR�BT�BR�BM�BE�BB�B@�B1'B�B/B2-B6FB2-B5?B9XB;dB?}BA�B@�B>wB=qB9XB6FB33B.B0!B%�B!�B �B#�B �B�BoB�B�B�BPB�B�B�B�B�B�B�BuB�BhB	7B\B
=B�B�B�B$�B%�B#�B#�B!�B�B�BVBVB�B�B�B�B�B�B�B{BVBJB��BVB�B{B�B�B{BPB
=B�BVB	7B�B�B%�B-B-B-B,B(�B$�B'�B&�B�B�B�B1'B1'B-B%�B'�B-B0!B1'B8RB49B/B9XB;dB=qB?}B<jBC�BE�BJ�BE�B?}BG�BE�BL�BW
BVBVBVBW
BW
B\)BdZBffBffBjBm�Bm�Bl�Bl�Br�Bw�By�By�B|�B}�Bz�By�B{�B�B�%B�B�B�7B�JB�VB�bB��B�hB�DB��B��B��B��B��B�B�B�B�B�B�B�B��B��B�B�?B�XB�^B�dB�jB��BÖBB��B�}B��BBȴB��B��B��B��B�B�B�
B�
B��B�#B�)B�)B�#B�/B�TB�mB�B�B��B��B��B��B��B��B��B��B	B	bB	{B	�B	uB	�B	�B	�B	�B	 �B	%�B	#�B	#�B	-B	33B	7LB	@�B	B�B	D�B	D�B	I�B	K�B	J�B	J�B	N�B	Q�B	R�B	S�B	W
B	YB	W
B	ZB	[#B	[#B	`BB	bNB	e`B	gmB	jB	k�B	jB	iyB	hsB	jB	m�B	s�B	u�B	t�B	s�B	u�B	w�B	y�B	y�B	z�B	}�B	|�B	}�B	�%B	�7B	�DB	�JB	�PB	�\B	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�?B	�RB	�dB	�dB	�dB	�^B	�^B	�XB	�dB	�qB	�qB	�}B	�}B	�}B	�}B	��B	��B	ÖB	ÖB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�/B	�;B	�;B	�;B	�5B	�)B	�5B	�HB	�NB	�NB	�HB	�TB	�`B	�fB	�mB	�fB	�mB	�fB	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
B

=B

=B
1B

=B

=B
JB
JB
PB
VB
PB
JB
DB
bB
hB
hB
hB
bB
bB
bB
hB
oB
{B
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
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
!�B
 �B
�B
�B
!�B
!�B
!�B
#�B
&�B
&�B
%�B
%�B
$�B
$�B
#�B
&�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
,B
-B
.B
-B
,B
-B
.B
.B
.B
-B
/B
0!B
1'B
1'B
2-B
2-B
2-B
1'B
0!B
1'B
0!B
1'B
0!B
1'B
33B
2-B
1'B
33B
49B
33B
33B
49B
49B
33B
33B
5?B
5?B
6FB
8RB
9XB
9XB
9XB
9XB
8RB
9XB
:^B
:^B
:^B
9XB
8RB
8RB
7LB
7LB
7LB
9XB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
<jB
=qB
?}B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
C�B
C�B
B�B
A�B
@�B
?}B
A�B
B�B
@�B
A�B
B�B
C�B
D�B
A�B
E�B
D�B
B�B
F�B
I�B
I�B
I�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
G�B
L�B
M�B
M�B
L�B
K�B
I�B
N�B
N�B
M�B
M�B
M�B
M�B
N�B
P�B
P�B
P�B
O�B
N�B
O�B
P�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
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
YB
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
YB
ZB
YB
XB
YB
YB
YB
YB
ZB
\)B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
^5B
`BB
`BB
`BB
_;B
_;B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
`BB
`BB
aHB
bNB
aHB
bNB
bNB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
cTB
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
e`B
dZB
cTB
cTB
e`B
e`B
ffB
e`B
ffB
ffB
gmB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
hsB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
jB
jB
iyB
jB
jB
k�B
l�B
m�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�4B�HB��B�B�!B��B~�B�B��B� BiBu�Bp�Bg�BVmBX�BG�B�BaB��B��B�OB�CB��B��B��B��B�1B��B�!B�YB�B��B~(BhsB^5BGEB:�B0;B�G�O�B
�-B
�B
�`B
�]B
��B
�hB
��B
�eB
��B
��B
g�B
tB
l�B
gRB
`'B
S�B
:DB
CB
2aB
-�B
$�B
�G�O�B	�*B	�B	�fB	�`B	�-B	�B	�;B	�B	�B	յB	�KB	�hB	��B	�3B	�B	��B	�B	�:B	��B	��B	�OB	�B	��B	�fB	��B	cB	gB	a�B	S@B	c�B	b�B	^�B	_�B	\�B	Y�B	]~B	a�B	\�B	RoB	K�B	F�B	A�B	:�B	=VB	<6B	2|B	3�B	+6B	"B	!�B	!bB	#TB	�B	9B	
�B	�B	�B	�B��B�%B��B�B�B�B�)B�B�_B�B��B�FBٚB�B�[BѷB��BǔB��B��B��B��B��B��B�B��B��B��B��B��B�kB��B��B�|B�G�O�B��B��B�jB��B~�B�iB��B��B��B��B�zB��B~�Bu�Bl=Bk�BpUBa�B[�BX�B`B`'BO�BH�BR BTBU�BS�BN�BF�BC�BA�B33B�B0�B3hB7fB3MB6FB:*B<B?�BA�B@�B>�B=�B9�B6�B4B/5B0�B'RB#B!�B$�B!�B�B,B�B;B�B�B;BOBkB \BCBeBEBFB?BoB
�B.BxBBYBOB%B&B$&B$B!�BBQB�BvBBB BBBBB�B(BB �BBBMB+B$BMB�BxB?BvB
�B/B �B&LB-)B-CB-)B,=B)DB%�B(XB'mB�B�B�B1[B1vB-�B'B(�B-�B0�B2B8�B5B0UB9�B<B>B@B=VBDBF?BKBFYB@�BHKBF�BMjBWYBVmBVmBV�BW�BW�B\�Bd�Bf�BgBj�Bm�Bm�Bl�Bm)Br�BxBzDBz*B}"B~(B{JBzxB|�B�mB�tB��B��B��B��B��B��B��B��B�JB�B�/B�\B�FB�KB�6B�6B�CB�IB�IB�CB�kB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�&B�&B�,B�9B�9B�$B�sB�gB�WB�xBܒB��B��B�B��B�B��B�B�*B�>B�`B�`B�"B�B��B	�B	�B	{B	�B	�B	�B	�B	�B	B	 �B	%�B	$&B	$ZB	-]B	3�B	7�B	@�B	B�B	D�B	D�B	I�B	K�B	J�B	J�B	OB	RB	S&B	TFB	W?B	Y1B	WYB	ZQB	[qB	[�B	`\B	b�B	ezB	g�B	j�B	k�B	j�B	i�B	h�B	j�B	m�B	s�B	u�B	t�B	s�B	u�B	w�B	y�B	zB	{B	~B	}<B	~]B	�?B	�lB	�^B	�dB	�jB	�vB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2B	�>B	�B	�"B	�6B	�=B	�"B	�B	�)B	�QB	�qB	�?B	�lB	�dB	�dB	�B	�xB	�xB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ðB	ŢB	żB	ŢB	żB	żB	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�,B	�B	�B	�B	�@B	�,B	�B	�,B	�,G�O�B	�9B	�7B	�kG�O�B	�IB	�;B	�pB	�VB	�jG�O�B	ބB	�bB	�hB	�hB	�|B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	��B	�B	��B	�$B	�RB	�"B	�.B	�.B
 4B
UB
-B
AB
AB
[B
gB
_G�O�B

XB

XG�O�B

XB

rB
dB
~B
jB
VB
jB
~B
�B
bB
hB
hB
�B
}B
}B
�B
�B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�G�O�B
�B
�B
�B
�B
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
!�B
 �G�O�B
B
!�B
!�B
"B
#�B
&�B
&�B
%�B
%�B
%B
%,B
$B
'B
)B
)B
*B
*B
*0B
*B
*0B
+B
,"B
-B
,=B
-)B
.IB
-)B
,=B
-)B
.IB
.IB
./B
-]B
/5B
0;B
1'B
1AB
2-B
2-B
2-B
1AB
0;B
1AB
0UB
1AB
0UB
1AB
3MB
2GB
1[B
3hB
4TB
3MB
3MB
4nB
4nB
3hB
3hB
5ZB
5tB
6`B
8lB
9XB
9rB
9rB
9XB
8lB
9rB
:^B
:xB
:^B
9rB
8lB
8lB
7�B
7�B
7�B
9�B
;�B
<�B
=�B
=�B
=�B
=�B
>�B
>�G�O�B
=�B
?�B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
C�B
C�B
B�B
A�B
@�B
?�B
A�B
B�G�O�B
A�B
B�B
C�B
D�G�O�B
E�B
D�G�O�B
F�B
I�B
I�B
I�B
IB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
G�B
L�B
M�B
M�B
MB
LG�O�B
N�B
N�B
M�B
M�B
M�B
NB
N�B
P�B
Q B
P�B
O�B
OB
PB
Q B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
SB
R B
S&B
TB
TB
U2B
UB
UB
VB
VB
VB
UB
U2B
W$B
X+B
XB
XB
X+B
X+B
XB
XB
XB
XB
YB
X+B
XB
W$B
W$B
W$B
W$B
W$B
XEB
Y1B
ZB
Y1B
XEB
Y1B
Y1B
YKB
YKB
Z7B
\]B
]IB
]IB
^5B
^OB
^OB
^OB
]IB
^OB
`\B
`BB
`BB
_;B
_VB
^OB
^OB
_VB
`\B
`BB
`\B
`\B
`\B
abB
`\B
`\B
`\B
abB
bNB
abB
bhB
bNB
abB
bhB
bhB
bhB
cnB
cTB
cTB
cTB
cTB
bhB
a|B
cnB
dtB
dtB
dtB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dtB
cnB
cnB
ezB
ezB
f�B
ezB
f�B
f�B
g�B
gmB
hsB
hsB
g�B
hsB
h�B
hsB
i�B
hsB
i�B
h�B
h�B
h�B
h�B
h�B
h�B
g�B
h�B
h�B
h�B
i�B
jB
jB
i�B
j�B
j�B
k�B
l�B
m�B
l�111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141111141111111111111111111111111111111111111111111111111111114114111111111111111111111111111111111114411111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111411114114111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803220037572018032200375720180322003757201806221239102018062212391020180622123910201804050436192018040504361920180405043619  JA  ARFMdecpA19c                                                                20180318093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180318003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180318003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180318003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180318003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180318003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180318003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180318003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180318003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180318003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20180318005516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180318153442  CV  JULD            G�O�G�O�F¡�                JM  ARSQJMQC2.0                                                                 20180319000000  CF  PSAL_ADJUSTED_QCC  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180321153757  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180321153757  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193619  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033910  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                