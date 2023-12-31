CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:18Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230518  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_023                   2C  D   APEX                            5368                            041511                          846 @�ld���1   @�le#�@3�-V�d�     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D� D�9�D�p D���D�3D�<�D���D��fD���D�9�D��3Dǹ�D��D�9�Dڌ�D��fD���D�,�D�|�D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A?�A_�A�A��
A��
A�
=A��
A��
A��
A��
B Q�B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`Q�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C\{C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��DxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DIDI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt�Dy��D�\D�8�D�o\D��)D��D�<)D��)D���D���D�8�D���DǸ�D�)D�8�Dڌ)D���D���D�,)D�|)D�u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�-A�+A�-A�1'A�+A�-A� �A�/A�-A�+A�;dA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�=qA�A�A�C�A�C�A��mA��
A�t�A�{Aŗ�A���A�n�A���A�C�A�t�A���A�$�A�\)A��HA��
A�t�A��A��7A���A�hsA��!A��;A�jA��FA��9A��-A�x�A�=qA���A��yA���A�^5A�9XA�S�A��;A�x�A��A�^5A��PA�x�A��!A�oA�hsA�;dA���A���A��A�ȴA��A�C�A�^5A��PA�oA��A�\)A�v�A�S�A���A���A�7LA�;dA�A�;dA��jA�=qA��A�bNA�A���A��HA�l�A�ȴA�hsA���A��`A���A+A}�Az��Aw�^ArjAq�Ao��An�RAk��Aj$�Ai&�Ag��Ae��Ad�DAb9XA_A\ffAZ��AX �AW��AWAU7LAP��AN�+AM;dAK��AJ  AHȴAG�hAE"�AC��AB{A?S�A=�A<=qA;XA9��A8�uA8 �A7p�A6r�A4r�A2��A1��A0��A/��A.Q�A-K�A,9XA+hsA*��A)��A'+A%A$ĜA$JA#�7A#+A"jA!ƨA�-A��A5?A��A��A �A+A��A�HA��A�+AffA�mAt�AVA�+A�yA��A&�A�^A�#A�DA�;A
1'A��A=qA�PA��AA�A �A��A��A��A �A��AA �@�+@�{@�z�@�;d@��!@�ff@��#@�?}@��m@���@�G�@���@�@�r�@���@���@�@�I�@�"�@��@�%@�bN@⟾@���@߮@ݡ�@��@���@ڧ�@�$�@ف@�(�@�;d@�"�@��@��T@�z�@��@�  @Ӯ@�"�@��@Ь@�l�@��@��#@�%@ˍP@�t�@���@�ff@�J@�x�@��@�%@ȼj@ȓu@���@�+@ƸR@�=q@�V@�|�@��y@°!@+@��-@�9X@��@�  @��;@�"�@���@�v�@�/@��@���@���@��^@�`B@��@��`@��@�I�@�t�@�@��@��!@��@���@�X@��@�9X@��@��F@���@���@�V@��T@��@�Ĝ@��m@�o@��R@�5?@�hs@�7L@���@��j@��@��@��H@��+@�^5@�M�@�$�@��h@��@���@���@�z�@�j@�bN@�Q�@�9X@��@��
@�\)@�
=@���@�n�@�ff@�E�@�@�`B@��@��@���@��@��
@��m@��@�  @��w@�dZ@�+@�"�@��y@�v�@���@���@���@��h@�&�@���@�Ĝ@��u@�Q�@��w@�t�@��y@�V@��7@�&�@�/@�/@��@�V@�%@���@��@��;@��w@��P@�K�@�o@���@�ff@�-@���@��^@��7@�G�@��@���@���@��@�A�@��@�  @��
@���@�S�@���@��!@��\@�ff@�5?@�-@�J@��^@�x�@�O�@�V@���@��@�z�@�9X@��
@���@���@�C�@�"�@��@���@���@��\@�~�@�V@�=q@�J@��@��T@��#@��^@���@��7@��@���@��@���@�j@�1@��@��
@��w@���@��P@��@�|�@�\)@�;d@��@���@�ff@��@��-@�X@�O�@�&�@�V@��`@���@��@�t�@�@���@�-@�@���@�O�@�?}@�&�@���@��D@�(�@��w@���@�dZ@�C�@�
=@���@��y@���@���@�ff@��@�J@���@��-@���@���@��7@�O�@���@��@���@�r�@�(�@���@z�\@s�@k�
@b^5@[�
@S�@J��@Fȴ@>$�@6��@/+@'�@"��@��@�y@@\)@��@	�^@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�-A�+A�-A�1'A�+A�-A� �A�/A�-A�+A�;dA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�=qA�A�A�C�A�C�A��mA��
A�t�A�{Aŗ�A���A�n�A���A�C�A�t�A���A�$�A�\)A��HA��
A�t�A��A��7A���A�hsA��!A��;A�jA��FA��9A��-A�x�A�=qA���A��yA���A�^5A�9XA�S�A��;A�x�A��A�^5A��PA�x�A��!A�oA�hsA�;dA���A���A��A�ȴA��A�C�A�^5A��PA�oA��A�\)A�v�A�S�A���A���A�7LA�;dA�A�;dA��jA�=qA��A�bNA�A���A��HA�l�A�ȴA�hsA���A��`A���A+A}�Az��Aw�^ArjAq�Ao��An�RAk��Aj$�Ai&�Ag��Ae��Ad�DAb9XA_A\ffAZ��AX �AW��AWAU7LAP��AN�+AM;dAK��AJ  AHȴAG�hAE"�AC��AB{A?S�A=�A<=qA;XA9��A8�uA8 �A7p�A6r�A4r�A2��A1��A0��A/��A.Q�A-K�A,9XA+hsA*��A)��A'+A%A$ĜA$JA#�7A#+A"jA!ƨA�-A��A5?A��A��A �A+A��A�HA��A�+AffA�mAt�AVA�+A�yA��A&�A�^A�#A�DA�;A
1'A��A=qA�PA��AA�A �A��A��A��A �A��AA �@�+@�{@�z�@�;d@��!@�ff@��#@�?}@��m@���@�G�@���@�@�r�@���@���@�@�I�@�"�@��@�%@�bN@⟾@���@߮@ݡ�@��@���@ڧ�@�$�@ف@�(�@�;d@�"�@��@��T@�z�@��@�  @Ӯ@�"�@��@Ь@�l�@��@��#@�%@ˍP@�t�@���@�ff@�J@�x�@��@�%@ȼj@ȓu@���@�+@ƸR@�=q@�V@�|�@��y@°!@+@��-@�9X@��@�  @��;@�"�@���@�v�@�/@��@���@���@��^@�`B@��@��`@��@�I�@�t�@�@��@��!@��@���@�X@��@�9X@��@��F@���@���@�V@��T@��@�Ĝ@��m@�o@��R@�5?@�hs@�7L@���@��j@��@��@��H@��+@�^5@�M�@�$�@��h@��@���@���@�z�@�j@�bN@�Q�@�9X@��@��
@�\)@�
=@���@�n�@�ff@�E�@�@�`B@��@��@���@��@��
@��m@��@�  @��w@�dZ@�+@�"�@��y@�v�@���@���@���@��h@�&�@���@�Ĝ@��u@�Q�@��w@�t�@��y@�V@��7@�&�@�/@�/@��@�V@�%@���@��@��;@��w@��P@�K�@�o@���@�ff@�-@���@��^@��7@�G�@��@���@���@��@�A�@��@�  @��
@���@�S�@���@��!@��\@�ff@�5?@�-@�J@��^@�x�@�O�@�V@���@��@�z�@�9X@��
@���@���@�C�@�"�@��@���@���@��\@�~�@�V@�=q@�J@��@��T@��#@��^@���@��7@��@���@��@���@�j@�1@��@��
@��w@���@��P@��@�|�@�\)@�;d@��@���@�ff@��@��-@�X@�O�@�&�@�V@��`@���@��@�t�@�@���@�-@�@���@�O�@�?}@�&�@���@��D@�(�@��w@���@�dZ@�C�@�
=@���@��y@���@���@�ff@��@�J@���@��-@���@���@��7@�O�@���@��@���@�r�@�(�@���@z�\@s�@k�
@b^5@[�
@S�@J��@Fȴ@>$�@6��@/+@'�@"��@��@�y@@\)@��@	�^@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBn�Bn�Bn�Bn�Bm�Bn�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bo�Bn�Bo�Bo�Bo�Bn�B_;B33B�BVB\BDBhBhB{BuBoBoBuBuBPB1BB��B��B��B��B�B�B�B�B�B�B�B�B�mB�NB��BƨB�jB�FB�!B��B��B��B�+Bn�BgmB[#BB�B33B0!B.B �BJB�`B�
B�?B��B�%Bo�BaHBN�BC�B6FB+B�B
��B
�B
�ZB
��B
ȴB
B
�dB
�B
��B
��B
�JB
�B
}�B
m�B
M�B
,B
�B
	7B	�B	��B	ƨB	�}B	�FB	��B	��B	��B	�uB	�+B	}�B	r�B	ffB	XB	m�B	k�B	gmB	dZB	W
B	49B	(�B	+B	 �B	�B	PB	B��B�B�B�mB�BB�B��B��BɺBǮBŢB��B�}B�jB�RB�?B�3B�3B�RB�?B�?B�'B�B��B��B��B�uB�uB�{B��B�uB�PB�7B�B}�By�Bs�Br�Bs�Bs�Bs�Br�Br�Bq�Bp�Bo�Bn�Bk�BiyBffBe`BcTBaHB`BBbNBbNB`BB`BB`BB_;BaHB`BB_;B_;B_;B`BBaHBcTBffBffBgmBdZBe`BgmBhsBhsBgmBffBdZBdZBbNBbNBgmBhsBiyBn�Bs�Bv�Bx�By�Bx�By�Bz�B{�B~�B�B�B�B�B�%B�1B�=B�=B�=B�DB�PB�VB�VB�VB�\B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�3B�9B�FB�jB�jB�qB�qB��B��B��BƨB��B��B��B��B�B�B�B�B�)B�ZB�sB�sB�yB�B�B�B��B��B��B��B��B	B	%B		7B	JB	PB	oB	�B	�B	�B	"�B	"�B	"�B	"�B	"�B	!�B	#�B	&�B	'�B	(�B	)�B	1'B	6FB	7LB	8RB	=qB	>wB	?}B	@�B	A�B	C�B	C�B	E�B	F�B	H�B	H�B	I�B	I�B	M�B	P�B	VB	YB	^5B	^5B	^5B	_;B	_;B	`BB	_;B	`BB	bNB	cTB	ffB	iyB	m�B	o�B	q�B	q�B	v�B	x�B	z�B	{�B	}�B	�B	�B	�%B	�DB	�PB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�FB	�XB	�dB	�jB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
JB
PB
PB
PB
VB
VB
PB
VB
VB
\B
\B
\B
hB
uB
�B
�B
#�B
(�B
49B
:^B
?}B
E�B
J�B
Q�B
XB
_;B
dZB
jB
n�B
q�B
u�B
w�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bn�Bn�Bn�Bn�Bm�Bn�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bo�Bn�Bo�Bo�Bo�Bn�B_?B39B�BXBbBJBmBkB�BtBtBtByBzBVB4BB��B��B��B��B�B�B�B�B�B�B�B�B�qB�SB��BƨB�qB�LB�%B��B��B��B�.Bn�BgqB[&BB�B33B0"B.B �BMB�cB�B�?B��B�)Bo�BaJBN�BC�B6JB+B�B
��B
�B
�\B
��B
ȸB
B
�hB
�B
��B
��B
�PB
�)B
}�B
m�B
M�B
,B
�B
	=B	�B	��B	ƴB	��B	�TB	��B	��B	��B	��B	�:B	~B	r�B	fuB	XB	m�B	k�B	g}B	dkB	WB	4IB	)B	+B	 �B	�B	cB	"B��B�B�B�B�WB�*B�B��B��B��BŷB��B��B�~B�gB�UB�JB�IB�gB�VB�UB�>B�B��B��B��B��B��B��B��B��B�gB�NB�0B~By�Bs�Br�Bs�Bs�Bs�Br�Br�Bq�Bp�Bo�Bn�Bk�Bi�Bf�BezBcmBacB`_BbgBbeB`]B`]B`^B_VBa_B`\B_RB_TB_UB`]BaaBcmBf�Bf~Bg�BdsBexBg�Bh�Bh�Bg�BfBdrBdrBbgBbfBg�Bh�Bi�Bn�Bs�Bv�Bx�By�Bx�By�Bz�B| BB�$B�0B�0B�6B�=B�HB�VB�TB�UB�\B�hB�lB�oB�mB�vB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�B�<B�HB�HB�OB�ZB�~B�}B��B��B��B��B��BƻB��B��B��B�B�B�$B�*B�0B�:B�lB�B�B�B�B�B�B��B��B��B��B�
B	!B	5B		HB	[B	cB	B	�B	�B	�B	"�B	"�B	"�B	"�B	"�B	!�B	#�B	&�B	'�B	)B	*B	16B	6UB	7[B	8`B	=�B	>�B	?�B	@�B	A�B	C�B	C�B	E�B	F�B	H�B	H�B	I�B	I�B	M�B	P�B	VB	Y'B	^CB	^FB	^BB	_HB	_IB	`NB	_GB	`NB	bZB	ccB	ftB	i�B	m�B	o�B	q�B	q�B	v�B	x�B	z�B	{�B	~B	�B	�B	�2B	�PB	�\B	�nB	�rB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�%B	�2B	�;B	�?B	�DB	�PB	�bB	�oB	�uB	�}B	��B	��B	��B	áB	ĩB	ħB	ŮB	ǻB	ǺB	ȽB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�!B	�B	�3B	�9B	�CB	�LB	�KB	�LB	�SB	�VB	�]B	�`B	�kB	�oB	�oB	�~B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
*B
&B
.B
.B
/B
-B
/B
6B
;B
;B
	@B
	AB

GB

FB

FB

FB
LB
RB
SB
XB
WB
YB
_B
_B
UB
`B
aB
dB
eB
eB
pB
B
�B
�B
#�B
(�B
4AB
:eB
?�B
E�B
J�B
Q�B
XB
_AB
d`B
j�B
n�B
q�B
u�B
w�B
{�B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230518  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                