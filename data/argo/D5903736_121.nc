CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-13T19:16:26Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150813191626  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               yA   AO  4051_7090_121                   2C  D   APEX                            5368                            041511                          846 @�g_Zt�1   @�g_��@3C�
=p��d^�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    yA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffBffB   B'��B0  B8  B@  BG��BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D��D�I�D�� D�� D���D�L�D�y�D�� D��D�@ D�i�D��3D� D�I�D�y�D��3D�3D�C3D�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�BQ�BQ�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B�(�B�B���B���B���B���B���B���B���B���B���B�(�B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df�RDgxRDg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�DnDn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dtk�Dyk�D�)D�H�D�\D��\D��)D�L)D�x�D��\D��D�?\D�h�D�D�\D�H�D�x�D�ҏD��D�B�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A�A��yA��;A�-A�&�A�+Aޛ�A�(�A�O�A�r�A�v�A�I�A���A��HA� �AϸRA�ZA�(�A��A�l�A̅Aʥ�A�+A�C�A��mA��`A��yAę�A�ȴA���A�(�A���A�ȴA��jA�A�A��A���A�A�7LA�O�A��uA�A�`BA��PA��A�\)A��A���A��yA���A�^5A�bNA�p�A��A�l�A�z�A�{A�n�A�\)A��9A���A��7A�ȴA��A�-A�&�A�%A��!A��A�G�A�"�A���A�S�A���A�7LA�K�A�5?A�"�A��;A��
A�v�A�v�A�A�A��A��A�ZA�-A�$�A��A�dZA��;A�n�A���A��`A���A�=qA�oA��!A��FA�E�A�mA}�wAy�-ArQ�AoAnM�Am7LAl{Aj�HAi%Aet�Abv�A_�hA]�A]�7A];dA\=qA[|�AY�^AX�AV�DAU%AR��AQt�APA�AOVANALv�AK/AJ1'AH��AF��AF$�AE�AE�PAD�RAA�A@�DA@-A?��A>��A<��A;�mA;O�A9ƨA7��A6��A5�mA4{A2A�A0��A0  A.��A-XA,�+A*�!A(9XA'33A%��A#?}A!�TA �HA VAO�A(�A=qAJAffAVA-A��A�A\)A�9AQ�A5?A&�A  A(�A�jA�A
n�A
�AI�A��A|�Ax�A�hA&�A��AƨA��A5?A\)A��AbA  A=qA�A ĜA bN@��@�G�@��/@��w@��@�hs@�;d@�9@�@�+@�/@�F@�
=@�$�@��@��@�V@�A�@�@��@�{@�hs@���@߾w@ޗ�@�\)@��@�/@��`@��m@��H@�J@�7L@�dZ@�@�hs@�V@�A�@�S�@·+@�hs@���@�b@�"�@�M�@�@ɡ�@�hs@�bN@��@�^5@�G�@���@�Ĝ@���@Ĵ9@���@ě�@��@�@��7@���@�%@�&�@��u@�Q�@�b@�l�@�@��@�Q�@���@�A�@�l�@���@��!@��R@���@�~�@�V@�{@��h@�%@�1'@��@�v�@�M�@�J@�M�@���@���@��7@���@�Z@�1'@���@���@�|�@�C�@���@���@���@�V@��#@��T@�-@�$�@�@��@��h@�7L@���@��@��P@�v�@�{@�G�@��@��@��@��/@�r�@�9X@�  @���@���@���@�V@��T@�O�@��9@��m@�l�@�S�@�C�@�;d@��R@�^5@���@��-@��7@�1'@��@��@���@�n�@�=q@�J@��#@�@���@��@��j@�Z@���@��y@���@�~�@�M�@��T@�x�@�G�@���@�z�@�1'@��m@��w@��w@��@�S�@��@��y@���@���@�ff@�V@�V@�$�@��@��@��^@���@��@���@� �@��;@��
@��m@���@��F@��@�l�@�K�@�o@���@�v�@��T@���@�  @��w@���@�5?@�n�@�E�@��@��^@�x�@�X@�O�@�?}@���@���@��j@��@��D@�1'@�"�@�@��R@�n�@�n�@�v�@��@���@�`B@��@��@�%@��`@��u@�9X@� �@��;@�+@��!@�E�@�{@�{@�@��^@���@���@���@���@���@�G�@��@���@���@���@�r�@�1@��
@��@��@�l�@�S�@�C�@��@��H@��@�ȴ@���@��y@��y@���@��\@�n�@�V@�{@���@�C�@{dZ@q��@f$�@^�y@X��@Q�#@I��@A�7@9��@2�@-�@'|�@#33@?}@�@��@�@S�@�P@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A�A��yA��;A�-A�&�A�+Aޛ�A�(�A�O�A�r�A�v�A�I�A���A��HA� �AϸRA�ZA�(�A��A�l�A̅Aʥ�A�+A�C�A��mA��`A��yAę�A�ȴA���A�(�A���A�ȴA��jA�A�A��A���A�A�7LA�O�A��uA�A�`BA��PA��A�\)A��A���A��yA���A�^5A�bNA�p�A��A�l�A�z�A�{A�n�A�\)A��9A���A��7A�ȴA��A�-A�&�A�%A��!A��A�G�A�"�A���A�S�A���A�7LA�K�A�5?A�"�A��;A��
A�v�A�v�A�A�A��A��A�ZA�-A�$�A��A�dZA��;A�n�A���A��`A���A�=qA�oA��!A��FA�E�A�mA}�wAy�-ArQ�AoAnM�Am7LAl{Aj�HAi%Aet�Abv�A_�hA]�A]�7A];dA\=qA[|�AY�^AX�AV�DAU%AR��AQt�APA�AOVANALv�AK/AJ1'AH��AF��AF$�AE�AE�PAD�RAA�A@�DA@-A?��A>��A<��A;�mA;O�A9ƨA7��A6��A5�mA4{A2A�A0��A0  A.��A-XA,�+A*�!A(9XA'33A%��A#?}A!�TA �HA VAO�A(�A=qAJAffAVA-A��A�A\)A�9AQ�A5?A&�A  A(�A�jA�A
n�A
�AI�A��A|�Ax�A�hA&�A��AƨA��A5?A\)A��AbA  A=qA�A ĜA bN@��@�G�@��/@��w@��@�hs@�;d@�9@�@�+@�/@�F@�
=@�$�@��@��@�V@�A�@�@��@�{@�hs@���@߾w@ޗ�@�\)@��@�/@��`@��m@��H@�J@�7L@�dZ@�@�hs@�V@�A�@�S�@·+@�hs@���@�b@�"�@�M�@�@ɡ�@�hs@�bN@��@�^5@�G�@���@�Ĝ@���@Ĵ9@���@ě�@��@�@��7@���@�%@�&�@��u@�Q�@�b@�l�@�@��@�Q�@���@�A�@�l�@���@��!@��R@���@�~�@�V@�{@��h@�%@�1'@��@�v�@�M�@�J@�M�@���@���@��7@���@�Z@�1'@���@���@�|�@�C�@���@���@���@�V@��#@��T@�-@�$�@�@��@��h@�7L@���@��@��P@�v�@�{@�G�@��@��@��@��/@�r�@�9X@�  @���@���@���@�V@��T@�O�@��9@��m@�l�@�S�@�C�@�;d@��R@�^5@���@��-@��7@�1'@��@��@���@�n�@�=q@�J@��#@�@���@��@��j@�Z@���@��y@���@�~�@�M�@��T@�x�@�G�@���@�z�@�1'@��m@��w@��w@��@�S�@��@��y@���@���@�ff@�V@�V@�$�@��@��@��^@���@��@���@� �@��;@��
@��m@���@��F@��@�l�@�K�@�o@���@�v�@��T@���@�  @��w@���@�5?@�n�@�E�@��@��^@�x�@�X@�O�@�?}@���@���@��j@��@��D@�1'@�"�@�@��R@�n�@�n�@�v�@��@���@�`B@��@��@�%@��`@��u@�9X@� �@��;@�+@��!@�E�@�{@�{@�@��^@���@���@���@���@���@�G�@��@���@���@���@�r�@�1@��
@��@��@�l�@�S�@�C�@��@��H@��@�ȴ@���@��y@��y@���@��\@�n�@�V@�{@���@�C�@{dZ@q��@f$�@^�y@X��@Q�#@I��@A�7@9��@2�@-�@'|�@#33@?}@�@��@�@S�@�P@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBB%B+B1B1BPBbBhB{B�B1'BL�Bn�Bl�B�JB�{B��B�B�B�RBƨB�/B�B	7B%B��B��B�B-BG�BbNBx�B}�B�PB��B�FB�dB�B�B��BBBB%BPB�B�BoB�B�B1B�B+B�B�wB�BȴB��B�XB��B��B�{B�B{�Br�BaHBW
BO�BE�BG�BG�BG�BI�BffB�=Bw�BcTBbNB_;BT�BH�B<jB+B�BB��B�B�sB%B�BB��B�FB�B��B��B_;B33B2-B0!B"�B
�B
��B
�1B
ffB
N�B
?}B
(�B	��B	�fB	�#B	��B	��B	�}B	�-B	��B	�DB	}�B	v�B	u�B	s�B	n�B	iyB	`BB	YB	P�B	F�B	>wB	7LB	33B	49B	1'B	,B	&�B	"�B	�B	\B	PB	
=B	%B��B�B�mB�`B�NB�HB�#B�/B�B��BȴBÖB�wB�XB�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�RB�dB�RB�?B�RB�XB�FB�-B��B��B�JB�B}�B�B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�LBÖBĜBĜBB��B�dB�XB�?B�B��B��B�{B�DB�7B�VB�VB�PB�JB�DB�JB�JB�JB�DB�JB�JB�DB�JB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�?B�XB�jB�wB��B��BÖBŢBǮBȴB��B��B��B��B��B��B��B�
B�B�B�#B�#B�B�#B�)B�BB�BB�TB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	+B	1B	
=B	DB	JB	PB	bB	hB	�B	�B	�B	�B	�B	�B	!�B	&�B	(�B	+B	-B	.B	0!B	33B	7LB	;dB	=qB	>wB	A�B	B�B	C�B	F�B	F�B	G�B	J�B	N�B	Q�B	VB	]/B	aHB	bNB	ffB	jB	m�B	p�B	t�B	w�B	y�B	y�B	z�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�7B	�DB	�bB	�hB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�9B	�9B	�XB	�qB	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�5B	�5B	�;B	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B

=B
DB
JB
JB
PB
PB
VB
VB
\B
bB
bB
�B
#�B
-B
5?B
:^B
@�B
F�B
L�B
R�B
XB
]/B
cTB
ffB
l�B
q�B
t�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B!B%B"B$B%B%B B!B B!B%B%B)B.B6B6BSBeBoBB�B1'BL�Bn�Bl�B�MB�}B��B�B�B�TBƩB�1B�B	7B'B��B��B�B-BG�BbOBx�B}�B�RB��B�LB�hB�B�B��BBB B)BVB�B�BuB�B�B7B�B/B�B�|B�BȹB��B�[B��B��B�~B�"B{�Br�BaGBWBO�BE�BG�BG�BG�BI�BfjB�@Bw�BcVBbPB_=BU BH�B<nB+B�BB��B�B�rB'B�BB��B�IB�B��B��B_=B34B20B0(B"�B
�B
��B
�6B
foB
N�B
?�B
(�B	��B	�rB	�.B	��B	��B	��B	�8B	��B	�SB	~B	v�B	u�B	s�B	n�B	i�B	`QB	Y'B	P�B	F�B	>�B	7\B	3EB	4JB	16B	,B	&�B	"�B	�B	mB	dB	
NB	7B�B�B�B�qB�dB�[B�7B�DB�2B��B��BíB��B�mB�JB�>B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�KB�fB�{B�fB�UB�hB�kB�ZB�AB��B��B�aB�4B~B�7B��B��B��B��B��B��B�B�B�B�B�B�B��B��B�`BìBĴBĳB¤B��B�zB�mB�VB�*B��B��B��B�]B�MB�lB�kB�eB�_B�[B�bB�_B�`B�[B�cB�`B�\B�aB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�7B�=B�IB�TB�kB��B��B��B��BêBŵB��B��B��B��B��B��B��B��B�B�!B�1B�1B�4B�2B�4B�6B�?B�UB�SB�gB�B�B��B�B�B�B��B��B��B��B��B��B��B�B	<B	AB	
PB	TB	YB	aB	sB	yB	�B	�B	�B	�B	�B	�B	!�B	&�B	)B	+B	-B	.'B	01B	3BB	7[B	;sB	=B	>�B	A�B	B�B	C�B	F�B	F�B	G�B	J�B	N�B	Q�B	VB	];B	aVB	b\B	fsB	j�B	m�B	p�B	t�B	w�B	y�B	y�B	z�B	z�B	|�B	~B	�B	�B	� B	�$B	�+B	�0B	�4B	�7B	�=B	�DB	�BB	�QB	�nB	�vB	�yB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�&B	�0B	�3B	�DB	�EB	�DB	�cB	�}B	��B	��B	B	ħB	ūB	ƱB	ǹB	ȾB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�*B	�3B	�FB	�?B	�AB	�CB	�TB	�WB	�aB	�jB	�kB	�qB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
 
B
B
B
B
B
B
%B
!B
/B
.B
'B
0B
4B
6B
5B
5B
6B
:B
:B
	@B

GB
NB
PB
RB
XB
VB
^B
^B
gB
kB
kB
�B
#�B
-B
5EB
:dB
@�B
F�B
L�B
R�B
XB
]6B
cZB
fnB
l�B
q�B
t�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150813191626    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150813191626  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150813191626  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                