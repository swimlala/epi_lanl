CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:31Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140731  20181024140731  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @�Ѥ�`�1   @�ѥ\�6�@56�t��c�r� Ĝ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx` D�K3D�j=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A?�A_�A~zA��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C�GC��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��DxRD�RD~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D\D\�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��DhxRDh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx^�D�J�D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��yA��A��A��mA��mA��A��A�oA�C�A�`BA�v�Aޙ�Aޡ�Aާ�AެAެAޮAޮAޮAޮAޮAޮAް!A޲-A޲-A޴9A޾wA޺^Aް!Aާ�Aޥ�Aޝ�A�p�A���AԋDA�ZA�n�A�r�A���Aˡ�A�bA�ffAț�A���A�jAŗ�A��AĴ9AēuA�\)A�&�A��A�?}A�I�A���A�"�A��+A���A���A�t�A�{A���A��A�ƨA��A�-A�=qA�$�A��A�ZA��-A�A�A�ȴA�A���A�1'A��^A��+A��A��A��A�~�A�jA���A��A��A���A�-A�v�A���A���A���A���A�1A�l�A�bA��7A��9A�VA���A��9A��mA��A�VA��7A�ƨA�&�A�?}A�A�\)A���A��hAƨA~(�A|bNAz  Av�DAr�+Aq&�Ao�mAkhsAhVAg�AfAd��Ad^5Ac��Ac�AbVA]�7AZ��AX�AV��AVAT��AT{ARVAPbAM?}AK�
AJ�/AI�AH�AG��AE%ADQ�AB=qA@��A?oA:�/A9A8�RA8z�A7O�A5VA2��A1?}A/�7A-%A+��A*bA)hsA)XA(�\A';dA&JA%+A$�yA#%A r�A�A��AE�Al�AffA�^AA�uAA?}A^5A�;A��A��A�^AI�AbA1A  A��A
�A	��A	t�A�!A5?A�
A��A1A7LAZAA�wAK�A1'A�A\)A ��A 1@�(�@�ȴ@��@��7@�j@��@�ƨ@���@���@�A�@���@�"�@��#@�@��@䛦@�bN@�(�@��@��@◍@�V@��@�b@�Q�@���@�v�@��@�(�@۾w@�|�@�S�@�o@�ff@�^5@���@�ƨ@֗�@� �@�1@υ@�K�@�~�@��@�`B@̋D@�z�@�Z@�ff@ɩ�@���@���@�\)@���@�ff@�M�@���@���@�
=@�M�@���@�Ĝ@���@���@�K�@��R@���@��D@� �@�@���@�~�@�$�@��h@�7L@��u@��@�S�@�+@��y@��@��!@���@�o@��@���@���@�~�@�$�@���@�O�@�O�@�hs@�p�@�O�@��/@��@��@�dZ@�@�v�@�x�@���@�`B@�&�@��@���@�b@�+@�M�@�p�@��@��`@�Ĝ@�z�@�A�@��;@�|�@�C�@�
=@��@���@��+@�M�@�{@��@��7@�?}@��@��j@�r�@�\)@���@�ff@�M�@��@���@�hs@��-@��T@��@��@��@���@�@��@�=q@���@�M�@��-@�hs@���@��
@��y@�-@�7L@�7L@�7L@�7L@�/@�7L@�7L@�7L@�O�@�hs@��@��9@��`@��j@���@��9@��D@��@�+@�"�@���@�5?@�{@��T@��@��9@�z�@� �@��P@�K�@��@�o@�o@�@�v�@�-@��@��@���@��@��#@���@���@�@��-@��-@��-@��h@��@���@��\@��y@�K�@��@��y@��!@�^5@�-@��T@���@��`@���@��@�r�@�r�@�r�@�bN@�Z@�Z@�Q�@�A�@�bN@�j@�bN@�1'@��
@�;d@���@�p�@��D@��m@��@��H@��\@��@�^5@���@�5?@��@��@��@�n�@��h@���@�A�@���@��@��@���@�C�@�
=@���@��H@���@���@�~�@�v�@�V@�E�@�=q@�$�@�J@��@���@���@���@��@�z�@� �@��w@���@��@�\)@�@��H@�ȴ@��R@��!@��R@��!@���@�v�@}S&@j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��yA��A��A��mA��mA��A��A�oA�C�A�`BA�v�Aޙ�Aޡ�Aާ�AެAެAޮAޮAޮAޮAޮAޮAް!A޲-A޲-A޴9A޾wA޺^Aް!Aާ�Aޥ�Aޝ�A�p�A���AԋDA�ZA�n�A�r�A���Aˡ�A�bA�ffAț�A���A�jAŗ�A��AĴ9AēuA�\)A�&�A��A�?}A�I�A���A�"�A��+A���A���A�t�A�{A���A��A�ƨA��A�-A�=qA�$�A��A�ZA��-A�A�A�ȴA�A���A�1'A��^A��+A��A��A��A�~�A�jA���A��A��A���A�-A�v�A���A���A���A���A�1A�l�A�bA��7A��9A�VA���A��9A��mA��A�VA��7A�ƨA�&�A�?}A�A�\)A���A��hAƨA~(�A|bNAz  Av�DAr�+Aq&�Ao�mAkhsAhVAg�AfAd��Ad^5Ac��Ac�AbVA]�7AZ��AX�AV��AVAT��AT{ARVAPbAM?}AK�
AJ�/AI�AH�AG��AE%ADQ�AB=qA@��A?oA:�/A9A8�RA8z�A7O�A5VA2��A1?}A/�7A-%A+��A*bA)hsA)XA(�\A';dA&JA%+A$�yA#%A r�A�A��AE�Al�AffA�^AA�uAA?}A^5A�;A��A��A�^AI�AbA1A  A��A
�A	��A	t�A�!A5?A�
A��A1A7LAZAA�wAK�A1'A�A\)A ��A 1@�(�@�ȴ@��@��7@�j@��@�ƨ@���@���@�A�@���@�"�@��#@�@��@䛦@�bN@�(�@��@��@◍@�V@��@�b@�Q�@���@�v�@��@�(�@۾w@�|�@�S�@�o@�ff@�^5@���@�ƨ@֗�@� �@�1@υ@�K�@�~�@��@�`B@̋D@�z�@�Z@�ff@ɩ�@���@���@�\)@���@�ff@�M�@���@���@�
=@�M�@���@�Ĝ@���@���@�K�@��R@���@��D@� �@�@���@�~�@�$�@��h@�7L@��u@��@�S�@�+@��y@��@��!@���@�o@��@���@���@�~�@�$�@���@�O�@�O�@�hs@�p�@�O�@��/@��@��@�dZ@�@�v�@�x�@���@�`B@�&�@��@���@�b@�+@�M�@�p�@��@��`@�Ĝ@�z�@�A�@��;@�|�@�C�@�
=@��@���@��+@�M�@�{@��@��7@�?}@��@��j@�r�@�\)@���@�ff@�M�@��@���@�hs@��-@��T@��@��@��@���@�@��@�=q@���@�M�@��-@�hs@���@��
@��y@�-@�7L@�7L@�7L@�7L@�/@�7L@�7L@�7L@�O�@�hs@��@��9@��`@��j@���@��9@��D@��@�+@�"�@���@�5?@�{@��T@��@��9@�z�@� �@��P@�K�@��@�o@�o@�@�v�@�-@��@��@���@��@��#@���@���@�@��-@��-@��-@��h@��@���@��\@��y@�K�@��@��y@��!@�^5@�-@��T@���@��`@���@��@�r�@�r�@�r�@�bN@�Z@�Z@�Q�@�A�@�bN@�j@�bN@�1'@��
@�;d@���@�p�@��D@��m@��@��H@��\@��@�^5@���@�5?@��@��@��@�n�@��h@���@�A�@���@��@��@���@�C�@�
=@���@��H@���@���@�~�@�v�@�V@�E�@�=q@�$�@�J@��@���@���@���@��@�z�@� �@��w@���@��@�\)@�@��H@�ȴ@��R@��!@��R@��!@���@�v�@}S&@j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:^B:^B<jB<jB;dB;dB;dB=qBF�BXBbNBn�B�B�=B�VB�\B�bB�hB�oB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�jB��B�B�BB�B{B&�B5?B:^BD�BN�BS�Bo�Bx�B{�B}�B~�B�B�B�=B�oB��B��B��B�uB��B��B��B��B�DB�=B�PB��B�7BgmBm�Bp�Bn�BiyBXB<jB'�B �B��B�/B�B��B��B��B��B��B�VB�DB�+Bv�BbNBW
BK�B5?B"�B�BuBPBB
�B
�NB
�B
��B
ƨB
�jB
�B
��B
�%B
r�B
jB
cTB
^5B
VB
C�B
,B
�B
hB	��B	�ZB	B	�FB	�B	��B	�B	}�B	|�B	x�B	z�B	z�B	v�B	n�B	\)B	J�B	?}B	7LB	2-B	.B	(�B	�B	PB��B�B�B�fB�BB�)B�#B�B�B��B��BŢB��B�wB�qB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�{B�VB�%B� B|�Bx�Bs�Bp�Bm�BjBgmBffBe`BdZBcTBbNB_;B^5B\)B[#B[#BZBXBW
BVBT�BR�BQ�BP�BO�BN�BO�BQ�BQ�BP�BP�BS�BYB^5B]/B]/B\)B[#B[#BZBXBT�BS�BS�BR�BO�BN�BQ�BVBW
BXBXBW
BXB_;BbNBdZBgmBiyBl�Bq�Bt�Br�Bo�Br�Bv�By�B}�B�B�B�=B�oB�{B�{B�VB�VB�hB�uB�uB�{B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�RB�wBǮB��B��B��B��B��B��B�
B�)B�BB�HB�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B	B	1B	
=B	PB	VB	bB	�B	�B	�B	 �B	'�B	.B	2-B	1'B	6FB	9XB	>wB	@�B	@�B	B�B	H�B	I�B	L�B	O�B	P�B	R�B	W
B	\)B	]/B	_;B	`BB	aHB	bNB	dZB	ffB	gmB	iyB	l�B	o�B	p�B	q�B	r�B	s�B	x�B	z�B	y�B	{�B	}�B	}�B	� B	�B	�%B	�1B	�7B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�9B	�3B	�-B	�!B	�-B	�3B	�'B	�'B	�-B	�-B	�?B	�?B	�FB	�RB	�XB	�^B	�^B	�^B	�XB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ƨB	��B	�
B	�B	�)B	�;B	�;B	�;B	�5B	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�5B	�BB	�`B	�yB	�B	�B	�yB	�fB	�TB	�HB	�;B	�5B	�/B	�5B	�/B	�;B	�BB	�HB	�HB	�NB	�TB	�mB	�mB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
gB
!|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:^B:^B<jB<jB;dB;dB;dB=qBF�BXBbNBn�B�B�=B�VB�\B�bB�hB�oB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�jB��B�B�BB�B{B&�B5?B:^BD�BN�BS�Bo�Bx�B{�B}�B~�B�B�B�=B�oB��B��B��B�uB��B��B��B��B�DB�=B�PB��B�7BgmBm�Bp�Bn�BiyBXB<jB'�B �B��B�/B�B��B��B��B��B��B�VB�DB�+Bv�BbNBW
BK�B5?B"�B�BuBPBB
�B
�NB
�B
��B
ƨB
�jB
�B
��B
�%B
r�B
jB
cTB
^5B
VB
C�B
,B
�B
hB	��B	�ZB	B	�FB	�B	��B	�B	}�B	|�B	x�B	z�B	z�B	v�B	n�B	\)B	J�B	?}B	7LB	2-B	.B	(�B	�B	PB��B�B�B�fB�BB�)B�#B�B�B��B��BŢB��B�wB�qB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�{B�VB�%B� B|�Bx�Bs�Bp�Bm�BjBgmBffBe`BdZBcTBbNB_;B^5B\)B[#B[#BZBXBW
BVBT�BR�BQ�BP�BO�BN�BO�BQ�BQ�BP�BP�BS�BYB^5B]/B]/B\)B[#B[#BZBXBT�BS�BS�BR�BO�BN�BQ�BVBW
BXBXBW
BXB_;BbNBdZBgmBiyBl�Bq�Bt�Br�Bo�Br�Bv�By�B}�B�B�B�=B�oB�{B�{B�VB�VB�hB�uB�uB�{B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�RB�wBǮB��B��B��B��B��B��B�
B�)B�BB�HB�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B	B	1B	
=B	PB	VB	bB	�B	�B	�B	 �B	'�B	.B	2-B	1'B	6FB	9XB	>wB	@�B	@�B	B�B	H�B	I�B	L�B	O�B	P�B	R�B	W
B	\)B	]/B	_;B	`BB	aHB	bNB	dZB	ffB	gmB	iyB	l�B	o�B	p�B	q�B	r�B	s�B	x�B	z�B	y�B	{�B	}�B	}�B	� B	�B	�%B	�1B	�7B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�9B	�3B	�-B	�!B	�-B	�3B	�'B	�'B	�-B	�-B	�?B	�?B	�FB	�RB	�XB	�^B	�^B	�^B	�XB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ƨB	��B	�
B	�B	�)B	�;B	�;B	�;B	�5B	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�5B	�BB	�`B	�yB	�B	�B	�yB	�fB	�TB	�HB	�;B	�5B	�/B	�5B	�/B	�;B	�BB	�HB	�HB	�NB	�TB	�mB	�mB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
gB
!|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140731                              AO  ARCAADJP                                                                    20181024140731    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140731  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140731  QCF$                G�O�G�O�G�O�0               