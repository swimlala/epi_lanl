CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-22T00:35:31Z creation;2018-06-22T00:35:38Z conversion to V3.1;2019-12-19T07:39:20Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180622003531  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_252                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�l6= 1   @�l6�-� @9��A [��dC�C�\�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du�RDv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�B�Dׂ�D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��)D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��)D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��Aǝ�A�hsA�G�A�(�A���A���A�1'A�z�A�{A��7A�VA�XA�ȴA��wA��
A��A��/A�XA�VA��A�/A��7A�XA��PA���A��A�{A�ffA��A���A���A��A��hA��
A��TA�7LA���A�=qA��A���A��A�$�A��TA��RA���A�n�A�M�A��A�{A���A��uA�A�A��A�=qA�~�A��PA��DA��\A���A���A���A�JA�5?A���A���A�M�A���A��A�33A��A��`A�VA�1'A��hA�E�A�oA���A���A���A��!A�n�A��yA��wA���A���A��A���A�+A~bA}+A| �A{C�Ayt�Av��As�Ar�/ArVAq��Aqx�Ao�TAoG�Ao�An�uAnE�Amx�Ajn�Ah5?Ag&�Af�uAex�Ac��Ab^5Aal�A_��A^��A\1AXAV�9AU��AUK�AT �AR��ARn�ARJAQ�7AQG�AP�AO7LAM�hAK|�AIC�AH��AH$�AG�
AG�wAG33AE|�AD�AB�ABJAA�FAAx�AA
=A?C�A=x�A;�^A;%A:I�A9�PA9�A8A�A7
=A6=qA5�wA5&�A4VA3��A3oA2~�A2�A1��A1�#A1�A1oA0�A0�A/+A.-A,��A,{A*�RA)��A)��A)"�A(�A(��A'�-A&�jA%VA#��A#|�A#l�A!�A ffA�^A��AdZAS�A��A�A/AĜAM�A�wA��A��A�!A�A"�A$�A�yA��An�AI�A1'A�A��AĜA�TA&�A9XAO�A5?AhsA�jAdZA
�A	�mA��A{A�A�HA5?A�A�A�;A�FA�A��AS�A�`Ax�A n�@�%@���@�J@�dZ@�=q@��9@��y@�r�@���@��@��#@�X@��@�F@��@�t�@�C�@�"�@�R@��@� �@�E�@�A�@�~�@��@�/@�j@�l�@���@�~�@��#@�?}@���@�`B@ԋD@�bN@�1'@� �@�1@ӶF@�S�@�n�@��@϶F@�~�@�t�@���@���@ə�@�p�@�`B@�O�@��@��
@�/@��y@���@��@���@��-@��@�X@���@��P@��h@��m@�C�@�{@�`B@���@�ƨ@���@���@�$�@���@�7L@�V@�Z@�\)@��@�ȴ@��@���@�Q�@�dZ@���@���@���@���@�Ĝ@�z�@�A�@� �@���@�+@���@��j@�I�@��@���@�x�@�?}@���@��;@�t�@�l�@�dZ@��@��@�?}@���@��`@�Ĝ@��D@��
@�M�@�X@��/@�Z@��@�ƨ@�|�@���@�t�@�M�@�X@�&�@���@��9@���@�bN@�b@��w@�K�@�o@�ȴ@��@��^@�x�@�O�@�?}@�/@��@�Ĝ@��9@��@���@��D@�Q�@��@�t�@�\)@�C�@��@���@�n�@��@�O�@�Z@��;@�l�@�\)@�C�@�~�@�hs@�G�@�G�@�7L@�/@��@�V@���@��j@��D@�bN@�I�@�b@��w@�C�@�@��!@�V@�5?@�-@�$�@�@��T@��^@���@�`B@���@��D@�I�@�1'@��@�1@�w@�@�P@K�@�@~�R@~V@~$�@~{@}�@|�/@|Z@|1@{��@{�m@{�m@{�m@{�
@{�
@{�
@{�F@{��@{��@{t�@{@z�!@z^5@z=q@zJ@y�@y��@yhs@yG�@yG�@y�@x��@x�`@xĜ@x�u@xQ�@x1'@xb@v��@v��@v{@u�@u�@u@up�@t�@t9X@s��@s@r��@r~�@q��@q�7@qx�@q7L@p��@p�@o�P@nȴ@n�+@nE�@n5?@m�h@m`B@m�@l�@l�j@l�@lI�@k��@kC�@j�!@iX@hĜ@h�u@h  @fȴ@fv�@fV@fV@fE�@f$�@f@e?}@d�@d�D@c��@c�F@c�@ct�@cdZ@c33@b�@b��@b�@a�#@a&�@`��@`r�@_�@^�R@^@]�-@]�@[ƨ@[C�@[33@["�@[o@[@Z�@Z��@ZM�@Y�@Y�@X�u@XQ�@X1'@Xb@W��@W�@W��@W�P@W|�@W;d@W;d@W;d@W�@W
=@W
=@V�y@V��@V$�@U�-@U?}@U?}@UO�@UV@T�@T�D@T1@S��@SS�@S"�@R��@R^5@R�@Q��@QG�@Q%@P��@PĜ@P��@PQ�@P1'@O��@O|�@O\)@OK�@N��@NE�@N@M�-@L�@L�@L��@L�D@Lz�@Lz�@Lj@L9X@KC�@J��@J~�@I��@I�@H�9@G�;@G�w@G��@G|�@G\)@G;d@Fȴ@F$�@E��@E�-@E�h@E`B@E�@D�@D�@D�D@DZ@D�@C�@B�H@B��@A��@A��@A��@A��@Ax�@AX@A&�@A%@@Ĝ@@1'@?��@?;d@>�+@>@=�-@=p�@=O�@=�@<��@<��@<�D@<z�@<j@<Z@<I�@<I�@<I�@<9X@<9X@<�@;�m@;��@:�@:-@9��@9�7@9%@8r�@8  @7�@7��@7�P@7+@6��@6�y@6�@6�@6ȴ@6�R@6��@6��@6v�@6ff@6V@6$�@5�T@5��@5�@4�/@4�j@4Z@4I�@4(�@41@3��@3ƨ@3�F@3�F@3��@333@2�\@2^5@2=q@1�^@1hs@1&�@0�`@0Ĝ@0�u@0r�@0bN@0A�@0 �@/|�@.�@.ȴ@.ȴ@.��@.�+@.v�@.E�@-@-�-@-�-@-��@-��@-�@,�@,j@,9X@,�@,1@+ƨ@+�@*��@*M�@*=q@*�@*�@*J@)��@)�#@)7L@(r�@( �@'�;@'��@&��@&ȴ@&�R@&��@&��@&�+@&v�@&ff@&V@&E�@&5?@&$�@%�@%@%�-@%�@%V@$��@$�j@$�D@$Z@#��@#�F@#��@#��@#��@#��@#��@#�@#dZ@#"�@"�H@"��@"�!@"^5@"J@!��@ ��@ �9@ �9@ �9@ �u@ A�@l�@��@�+@$�@�T@p�@?}@��@j@��@�@t�@t�@t�@t�@dZ@S�@33@@��@~�@-@��@��@G�@�@��@ �@b@  @K�@��@v�@ff@ff@E�@{@�h@`B@�@V@�@�j@�@j@9X@1@ƨ@��@t�@S�@S�@C�@o@�!@n�@J@��@�7@hs@X@G�@7L@&�@%@%@%@��@Ĝ@Ĝ@�9@�9@�9@�9@��@��@�@Q�@�@�@��@�P@l�@\)@
=@ȴ@��@�+@�+@�+@�+@v�@5?@{@@�T@��@�-@�h@`B@V@��@��@��@��@�@�@�/@��@z�@��@�
@�F@��@��@�F@ƨ@�F@�F@�@o@
�@
�@
��@
�!@
�\@
n�@
n�@
=q@
=q@
-@
-@	��@	�#@	��@	��@	X@	G�@	7L@	7L@	&�@	�@	�@�`@��@�9@�@bN@bN@Q�@A�@1'@  @�@�;@�;@��@�w@�P@K�@
=@�y@�@��@v�@E�@{@@�@��@@@�h@�@p�@O�@�@��@��@�@�D@z�@z�@j@I�@9X@�@��@�m@�m@ƨ@��@��@dZ@33@�@�!@��@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��Aǝ�A�hsA�G�A�(�A���A���A�1'A�z�A�{A��7A�VA�XA�ȴA��wA��
A��A��/A�XA�VA��A�/A��7A�XA��PA���A��A�{A�ffA��A���A���A��A��hA��
A��TA�7LA���A�=qA��A���A��A�$�A��TA��RA���A�n�A�M�A��A�{A���A��uA�A�A��A�=qA�~�A��PA��DA��\A���A���A���A�JA�5?A���A���A�M�A���A��A�33A��A��`A�VA�1'A��hA�E�A�oA���A���A���A��!A�n�A��yA��wA���A���A��A���A�+A~bA}+A| �A{C�Ayt�Av��As�Ar�/ArVAq��Aqx�Ao�TAoG�Ao�An�uAnE�Amx�Ajn�Ah5?Ag&�Af�uAex�Ac��Ab^5Aal�A_��A^��A\1AXAV�9AU��AUK�AT �AR��ARn�ARJAQ�7AQG�AP�AO7LAM�hAK|�AIC�AH��AH$�AG�
AG�wAG33AE|�AD�AB�ABJAA�FAAx�AA
=A?C�A=x�A;�^A;%A:I�A9�PA9�A8A�A7
=A6=qA5�wA5&�A4VA3��A3oA2~�A2�A1��A1�#A1�A1oA0�A0�A/+A.-A,��A,{A*�RA)��A)��A)"�A(�A(��A'�-A&�jA%VA#��A#|�A#l�A!�A ffA�^A��AdZAS�A��A�A/AĜAM�A�wA��A��A�!A�A"�A$�A�yA��An�AI�A1'A�A��AĜA�TA&�A9XAO�A5?AhsA�jAdZA
�A	�mA��A{A�A�HA5?A�A�A�;A�FA�A��AS�A�`Ax�A n�@�%@���@�J@�dZ@�=q@��9@��y@�r�@���@��@��#@�X@��@�F@��@�t�@�C�@�"�@�R@��@� �@�E�@�A�@�~�@��@�/@�j@�l�@���@�~�@��#@�?}@���@�`B@ԋD@�bN@�1'@� �@�1@ӶF@�S�@�n�@��@϶F@�~�@�t�@���@���@ə�@�p�@�`B@�O�@��@��
@�/@��y@���@��@���@��-@��@�X@���@��P@��h@��m@�C�@�{@�`B@���@�ƨ@���@���@�$�@���@�7L@�V@�Z@�\)@��@�ȴ@��@���@�Q�@�dZ@���@���@���@���@�Ĝ@�z�@�A�@� �@���@�+@���@��j@�I�@��@���@�x�@�?}@���@��;@�t�@�l�@�dZ@��@��@�?}@���@��`@�Ĝ@��D@��
@�M�@�X@��/@�Z@��@�ƨ@�|�@���@�t�@�M�@�X@�&�@���@��9@���@�bN@�b@��w@�K�@�o@�ȴ@��@��^@�x�@�O�@�?}@�/@��@�Ĝ@��9@��@���@��D@�Q�@��@�t�@�\)@�C�@��@���@�n�@��@�O�@�Z@��;@�l�@�\)@�C�@�~�@�hs@�G�@�G�@�7L@�/@��@�V@���@��j@��D@�bN@�I�@�b@��w@�C�@�@��!@�V@�5?@�-@�$�@�@��T@��^@���@�`B@���@��D@�I�@�1'@��@�1@�w@�@�P@K�@�@~�R@~V@~$�@~{@}�@|�/@|Z@|1@{��@{�m@{�m@{�m@{�
@{�
@{�
@{�F@{��@{��@{t�@{@z�!@z^5@z=q@zJ@y�@y��@yhs@yG�@yG�@y�@x��@x�`@xĜ@x�u@xQ�@x1'@xb@v��@v��@v{@u�@u�@u@up�@t�@t9X@s��@s@r��@r~�@q��@q�7@qx�@q7L@p��@p�@o�P@nȴ@n�+@nE�@n5?@m�h@m`B@m�@l�@l�j@l�@lI�@k��@kC�@j�!@iX@hĜ@h�u@h  @fȴ@fv�@fV@fV@fE�@f$�@f@e?}@d�@d�D@c��@c�F@c�@ct�@cdZ@c33@b�@b��@b�@a�#@a&�@`��@`r�@_�@^�R@^@]�-@]�@[ƨ@[C�@[33@["�@[o@[@Z�@Z��@ZM�@Y�@Y�@X�u@XQ�@X1'@Xb@W��@W�@W��@W�P@W|�@W;d@W;d@W;d@W�@W
=@W
=@V�y@V��@V$�@U�-@U?}@U?}@UO�@UV@T�@T�D@T1@S��@SS�@S"�@R��@R^5@R�@Q��@QG�@Q%@P��@PĜ@P��@PQ�@P1'@O��@O|�@O\)@OK�@N��@NE�@N@M�-@L�@L�@L��@L�D@Lz�@Lz�@Lj@L9X@KC�@J��@J~�@I��@I�@H�9@G�;@G�w@G��@G|�@G\)@G;d@Fȴ@F$�@E��@E�-@E�h@E`B@E�@D�@D�@D�D@DZ@D�@C�@B�H@B��@A��@A��@A��@A��@Ax�@AX@A&�@A%@@Ĝ@@1'@?��@?;d@>�+@>@=�-@=p�@=O�@=�@<��@<��@<�D@<z�@<j@<Z@<I�@<I�@<I�@<9X@<9X@<�@;�m@;��@:�@:-@9��@9�7@9%@8r�@8  @7�@7��@7�P@7+@6��@6�y@6�@6�@6ȴ@6�R@6��@6��@6v�@6ff@6V@6$�@5�T@5��@5�@4�/@4�j@4Z@4I�@4(�@41@3��@3ƨ@3�F@3�F@3��@333@2�\@2^5@2=q@1�^@1hs@1&�@0�`@0Ĝ@0�u@0r�@0bN@0A�@0 �@/|�@.�@.ȴ@.ȴ@.��@.�+@.v�@.E�@-@-�-@-�-@-��@-��@-�@,�@,j@,9X@,�@,1@+ƨ@+�@*��@*M�@*=q@*�@*�@*J@)��@)�#@)7L@(r�@( �@'�;@'��@&��@&ȴ@&�R@&��@&��@&�+@&v�@&ff@&V@&E�@&5?@&$�@%�@%@%�-@%�@%V@$��@$�j@$�D@$Z@#��@#�F@#��@#��@#��@#��@#��@#�@#dZ@#"�@"�H@"��@"�!@"^5@"J@!��@ ��@ �9@ �9@ �9@ �u@ A�@l�@��@�+@$�@�T@p�@?}@��@j@��@�@t�@t�@t�@t�@dZ@S�@33@@��@~�@-@��@��@G�@�@��@ �@b@  @K�@��@v�@ff@ff@E�@{@�h@`B@�@V@�@�j@�@j@9X@1@ƨ@��@t�@S�@S�@C�@o@�!@n�@J@��@�7@hs@X@G�@7L@&�@%@%@%@��@Ĝ@Ĝ@�9@�9@�9@�9@��@��@�@Q�@�@�@��@�P@l�@\)@
=@ȴ@��@�+@�+@�+@�+@v�@5?@{@@�T@��@�-@�h@`B@V@��@��@��@��@�@�@�/@��@z�@��@�
@�F@��@��@�F@ƨ@�F@�F@�@o@
�@
�@
��@
�!@
�\@
n�@
n�@
=q@
=q@
-@
-@	��@	�#@	��@	��@	X@	G�@	7L@	7L@	&�@	�@	�@�`@��@�9@�@bN@bN@Q�@A�@1'@  @�@�;@�;@��@�w@�P@K�@
=@�y@�@��@v�@E�@{@@�@��@@@�h@�@p�@O�@�@��@��@�@�D@z�@z�@j@I�@9X@�@��@�m@�m@ƨ@��@��@dZ@33@�@�!@��@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��BĜBƨB��B�?B�oBk�BW
B�%B��B��B��B��B�=BgmB�PB�BS�B�B�-B�jB�!B��B��B�B�uB�B��B��B��B��Bw�BZB5?BJ�B^5BO�BH�B.B�B�B33B6FB7LB6FB33B2-B/B.B)�B�BuBVB�BǮB��B��B�jB�B��B� B�1B`BBz�B� Bs�Be`BK�BA�B�B�B
�sB
��B
��B
��B
��B
�B
�
B
�^B
��B
�jB
��B
}�B
�7B
�\B
�+B
hsB
iyB
H�B
YB
G�B
>wB
!�B
B	��B
�B
�B
{B
bB	��B	��B
1B	��B	�B	�)B	�-B	�B	�wB	�jB	�B	��B	�hB	�uB	v�B	p�B	N�B	+B	R�B	[#B	[#B	P�B	G�B	R�B	P�B	L�B	G�B	>wB	 �B	�B	DB	B	�B	�B	�B	�B	DB�B�B�B��B��B��B�B��B��BÖB��B��B��B��B��B�XB�dB��B�}B�FB�FB�^B�RB�^B�qB�jB�FB�'B�B��B��B�{B�B�uB�=B�+B��B�bB�hB�DBx�Bn�BgmBdZB{�Bv�B^5BVBk�Bu�Br�Bp�BdZB[#BbNBcTB`BBZBP�BI�BD�BB�BN�BF�B@�BT�BXBW
BVBO�BI�B=qB8RB8RB1'B+B,B)�B+B�B'�B%�B�B&�B)�B(�B(�B#�B$�B(�B5?B6FB2-B+B�B	7BbB  BJB�B
=B�B\B
=BB+BPBoB"�B$�B!�B-B,B)�B(�B"�B�BbBVBJB�B#�B#�B �B �B%�B$�B�B�B{BbB!�B.B.B.B,B'�B!�B�BPB!�B�BVB�B49B5?B33B33B/B'�B�B
=BuB'�B-B.B'�B>wB<jB49B/B(�B/B;dB:^BA�BC�B?}BD�BK�BI�BI�BL�BM�BH�BF�BN�BP�BJ�BD�BP�BK�BK�BZBffBffBdZBbNBcTBcTB^5B]/BVB^5BgmBdZB_;Br�Bu�Bs�Bs�B{�B�B~�By�Bu�B}�B�B�1B�%B�B{�Bw�B�B�VB�hB�uB�oB�\B�B�B��B��B�XB�^B�qB�wB�jB�jB�wB��BÖBĜBÖB��B��B��B��B��B��B�B�B�B�B�B��B��B�)B�;B�5B�/B�;B�HB�;B�BB�ZB�B�B��B��B�B��B		7B	DB	DB	DB	DB	DB	DB	
=B	PB	\B	hB	bB	hB	{B	�B	�B	!�B	(�B	+B	+B	+B	,B	-B	.B	/B	0!B	33B	:^B	>wB	@�B	A�B	A�B	D�B	D�B	D�B	E�B	E�B	G�B	J�B	J�B	I�B	L�B	Q�B	VB	YB	ZB	ZB	ZB	ZB	[#B	[#B	ZB	\)B	[#B	\)B	[#B	_;B	bNB	e`B	e`B	ffB	gmB	iyB	k�B	l�B	l�B	m�B	m�B	m�B	n�B	n�B	o�B	o�B	l�B	t�B	v�B	z�B	{�B	z�B	y�B	y�B	{�B	� B	�B	�+B	�1B	�1B	�VB	�\B	�VB	�VB	�bB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�9B	�9B	�9B	�}B	B	ÖB	ÖB	B	B	��B	ÖB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�5B	�ZB	�ZB	�ZB	�ZB	�TB	�HB	�NB	�HB	�HB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B	��B
B
B
B
B
%B
+B
JB
PB
JB
PB
PB
DB
DB
VB
bB
bB
bB
bB
hB
hB
oB
hB
hB
bB
bB
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
 �B
�B
�B
�B
�B
 �B
"�B
 �B
"�B
#�B
&�B
&�B
&�B
&�B
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
(�B
'�B
(�B
(�B
)�B
,B
+B
-B
-B
-B
-B
-B
.B
.B
,B
+B
)�B
.B
.B
-B
/B
/B
0!B
1'B
1'B
2-B
1'B
1'B
1'B
/B
/B
49B
5?B
49B
49B
49B
49B
33B
6FB
6FB
6FB
5?B
49B
33B
33B
6FB
7LB
7LB
6FB
5?B
49B
6FB
:^B
:^B
:^B
:^B
:^B
8RB
6FB
6FB
;dB
;dB
;dB
;dB
>wB
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
?}B
@�B
@�B
>wB
@�B
A�B
A�B
@�B
@�B
A�B
C�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
E�B
G�B
G�B
E�B
D�B
A�B
E�B
F�B
G�B
H�B
G�B
I�B
H�B
I�B
H�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
O�B
N�B
N�B
Q�B
P�B
N�B
O�B
T�B
T�B
T�B
T�B
S�B
R�B
T�B
VB
W
B
W
B
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
XB
YB
YB
ZB
YB
XB
W
B
XB
XB
ZB
[#B
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
^5B
^5B
^5B
^5B
]/B
\)B
\)B
^5B
_;B
_;B
_;B
_;B
^5B
_;B
aHB
aHB
bNB
bNB
aHB
aHB
`BB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
bNB
bNB
e`B
e`B
ffB
gmB
gmB
gmB
ffB
e`B
e`B
dZB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
jB
jB
jB
jB
jB
jB
iyB
jB
jB
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
k�B
k�B
k�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
p�B
p�B
p�B
q�B
r�B
s�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B��B�+B�MBo�B\�B��B��B�*B�jB�jB�jBl�B��B�zB[�B��B��B��B��B�sB��B��B�B��B��B��B��B�SBz^B]~B:�BL�B_pBQNBJ#B1B�B5B4B6�B7�B6�B3�B2|B/�B.IB*eB�BaB(B�B˒B�B��B�wB��B�fB�-B��Bd@B{�B��Bt�Bf�BNBC�B�B�B
��B
��B
�*B
��B
�^B
�B
�KB
��B
�"B
��B
��B
��B
��B
��B
�B
jeB
j�B
K�B
ZB
IB
?�B
$ZB
�B	�*B
~B
CB
MB
 B	��B	��B
fB	��B	�[B	�B	��B	��B	��B	�VB	��B	��B	�@B	��B	y	B	r|B	RoB	/�B	TaB	\CB	[�B	RTB	IB	S�B	Q�B	M�B	HKB	?}B	#B	�B	�B	�B	 �B	VB	B	B	~B��B�nB�iB��B�qB�XB�B�gB�BŢB��B��B��BΥB��B��B��B�AB�iB��B�LB�B�	B��B��B��B��B��B��B��B�	B�B�3B�,B��B�KB�
B�B��B��Bz^Bp;Bi�BfB|6BwfB`\BXBl=BvBsBp�BezB\]BcBc�B`�B[	BR BKDBFYBD3BO�BHBB'BUMBX_BWYBVSBPbBJrB>�B9�B9rB2�B,qB-�B+6B,B�B)*B'B!-B'�B*�B)�B)�B$�B&2B)�B5tB6zB2|B+�B �BDB�B�B�B]BB?B}B�B�B�B�B�B#:B%`B"�B-)B,WB*KB)*B#nBeB�B�B�B�B$ZB$�B!|B!|B&fB%`B vBOB�B�B"NB./B.IB.IB,=B(XB"hB�B�B"NB�BbB�B4nB5�B3�B3MB/iB(sB�B0B�B(�B-�B.�B)DB>�B<�B5B0!B*eB0;B;�B;JBBBD3B@OBE9BLBJ=BJ#BMBN"BIRBGzBO(BQ4BKxBE�BQhBL�BL�BZ�Bf�Bf�Bd�Bb�Bc�Bc�B^�B]�BW?B^�Bg�BeB`vBr�BvBt9BtTB|PB� B.Bz^Bv�B~wB�SB�KB�YB�uB|�Bx�B��B��B��B��B��B��B��B��B��B��B�rB��B��B��B��B��B��B��B��B��B�B�B�B� B�B�B�&B�B�+B�+B�EB�9B�MBӏB�]B�pB�jB�~BߊB�B��B��B�B�B��B��B�2B�hB�zB		7B	DB	^B	^B	^B	^B	^B	
rB	�B	�B	�B	�B	�B	�B	�B	B	"B	)B	+B	+B	+6B	,=B	-CB	.IB	/iB	0oB	3�B	:�B	>�B	@�B	A�B	A�B	D�B	D�B	D�B	E�B	E�B	G�B	J�B	J�B	J	B	MB	R B	VB	Y1B	Z7B	ZB	ZB	Z7B	[=B	[#B	Z7B	\CB	[WB	\CB	[WB	_VB	bhB	ezB	ezB	f�B	g�B	i�B	k�B	l�B	l�B	m�B	m�B	m�B	n�B	n�B	o�B	o�B	mB	t�B	v�B	z�B	{�B	z�B	zB	z*B	|6B	�4B	�GB	�EB	�fB	��B	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�8B	�8B	�0B	�KB	�B	�UB	�TB	��B	��B	��B	B	ÖB	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�B	�B	�"B	�jB	�&B	�?B	�SB	ևB	�OB	�tB	�tB	�tB	�ZB	�nB	�|B	�B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�0B	�(B	�(B	�<B
'B
B
3B
B
-B
'B
;B	�BB
;B
GB
oB
gB
YB
zB
JB
jB
dB
jB
jB
xB
xB
pB
}B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
 �B
�B
�B
�B
B
 �B
"�B
!B
#B
$B
&�B
'B
'B
'B
)B
)�B
)�B
)�B
)�B
)�B
*B
*B
*B
)�B
*B
)B
($B
)B
)DB
*B
,"B
+QB
-B
-CB
-)B
-)B
-)B
.B
.B
,=B
+6B
*KB
./B
./B
-CB
/5B
/5B
0;B
1AB
1AB
2-B
1AB
1AB
1AB
/�B
/OB
4TB
5?B
4TB
4TB
4TB
4TB
3�B
6`B
6FB
6`B
5?B
4TB
3�B
3hB
6`B
7�B
7fB
6`B
5tB
4�B
6�B
:^B
:xB
:^B
:xB
:^B
8lB
6�B
6�B
;B
;B
;B
;�B
>�B
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
?�B
@�B
@�B
>�B
@�B
A�B
A�B
@�B
@�B
A�B
C�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
E�B
G�B
G�B
E�B
D�B
A�B
E�B
F�B
G�B
H�B
G�B
I�B
H�B
J	B
IB
N�B
N�B
O�B
O�B
N�B
N�B
N�B
M�B
NB
M�B
M�B
M�B
N�B
N�B
M�B
O�B
OB
OB
Q�B
Q B
O(B
P.B
T�B
T�B
T�B
UB
TB
S&B
UB
VB
W
B
W$B
VB
W$B
W?B
W$B
W$B
W$B
XEB
Y1B
YKB
ZB
YKB
X+B
W?B
X+B
XEB
Z7B
[=B
\)B
\)B
\CB
]/B
]/B
]IB
]/B
]/B
]/B
]IB
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]IB
\CB
\CB
^OB
_;B
_;B
_VB
_pB
^OB
_VB
aHB
aHB
bNB
bNB
aHB
abB
`vB
bNB
bNB
bhB
bNB
b�B
bhB
bhB
bhB
dZB
dZB
dZB
dZB
dZB
dZB
cnB
cnB
bhB
b�B
ezB
ezB
ffB
gmB
gmB
g�B
ffB
ezB
ezB
d�B
ffB
gmB
g�B
g�B
g�B
g�B
hsB
g�B
hsB
hsB
h�B
g�B
h�B
h�B
h�B
h�B
jB
jB
jB
jB
jB
jB
i�B
jB
j�B
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
k�B
k�B
k�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
p�B
p�B
p�B
q�B
r�B
s�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806260033442018062600334420180626003344201806260200212018062602002120180626020021201806270024062018062700240620180627002406  JA  ARFMdecpA19c                                                                20180622093525  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180622003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180622003536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180622003537  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180622003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180622003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180622003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180622003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180622003538  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180622003538                      G�O�G�O�G�O�                JA  ARUP                                                                        20180622005722                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180622153506  CV  JULD            G�O�G�O�F�a�                JM  ARCAJMQC2.0                                                                 20180625153344  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180625153344  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180625170021  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180626152406  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                