CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:08Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230508  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_005                   2C  D   APEX                            5368                            041511                          846 @�>O����1   @�>P�?�@3��C���d\Z�11   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds� Dy�fD�3D�0 D�|�D���D��D�I�D��3D��fD�fD�6fD�� D��3D�� D�Y�D�|�D���D�  D�FfD�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds޸Dy�D��D�/\D�|)D��)D�)D�H�D���D���D��D�5�D��\D�ҏD��\D�X�D�|)D��)D��\D�E�D�r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˕�A˓uA˓uAˑhA˕�A�I�A�|�A�5?A�"�A��A��A�{A�VA�JA�
=A�%A�  A���A���A���A���A���A���A���A�  A�A�1A�1A�1A�1A�
=A�
=A�%A�A�  A���A���A���A��`A�1Aơ�A�A�t�Ağ�A�"�A�5?A�z�A�  A�l�A���A��+A�Q�A��\A�&�A��uA��A��\A���A���A��A�1'A�  A��7A�G�A��A�G�A��!A��A�dZA�-A�^5A�A�l�A���A�?}A���A�A��A��HA�1'A�$�A��PA�p�A�{A���A��RA�dZA�VA�A�A���A��\A��jA�VA��A���A�A�^5A���A�VA��RA��A�+A���A��A��FA�{A���A�XA�%A� �A�A�A�VA��FA��;A~1Ay�Ay7LAu�^AtE�AsVAq�Ap��Ao"�Am��AlbNAk�PAgS�Ad��Ab��A`��A_��A^A\(�AZ��AY��AX��AV�`AT�HASXAQ��AP��AO��AM��AK`BAI�AHjAG�AF�AE�
AC�;AA�A@-A?��A?hsA>~�A<��A;��A:��A:5?A9l�A7�-A6n�A5�;A3�
A2��A1�A133A/��A/��A/VA.1A-S�A+�;A*�uA(��A'�7A'33A&�yA%��A#�A#��A"{A��AG�A�Av�A�wA{A|�A`BA?}A%A�!A1AhsA��A�A"�A��A�A��A�wA�!A^5A�hA�Ar�AffAA�A33A
VA	�TA	XA�HA��AS�An�A/AI�A�AO�A�A��AJAC�A ĜA {@�o@���@�^5@�V@�x�@�9X@�|�@���@��@�V@�V@�p�@�%@�(�@�n�@�J@�9X@�+@�O�@�I�@�1@�!@� �@���@ߕ�@�@��@ى7@���@�  @ׅ@�;d@�ff@���@�b@�9X@�r�@���@�O�@���@Ѓ@�I�@�(�@� �@�  @��@Ϯ@�t�@�
=@�~�@͑h@˥�@�o@ʸR@�^5@��T@ɡ�@�&�@�C�@���@��@�Ĝ@ģ�@�bN@öF@��@���@�@�p�@�1'@��@�C�@���@�x�@�t�@��!@�V@�X@��@�(�@�"�@�$�@�hs@�&�@���@�(�@�1@�9X@���@��@�O�@�V@�Z@��m@�;d@�5?@�X@���@�r�@��@��@�|�@�t�@�l�@���@�1@�t�@�33@�@�~�@��+@��+@�v�@��@�?}@� �@��;@��F@�t�@�o@�=q@��#@��@�O�@��`@�9X@��m@��P@�C�@��H@��\@�n�@�-@��@���@��7@�&�@���@��@� �@��;@���@�@�~�@�-@��T@��^@���@��7@�hs@�/@��`@��D@�I�@�b@��@��F@��P@�\)@�;d@�@��H@�-@��#@���@�x�@�&�@��`@�Ĝ@��@�I�@��@�+@���@���@�n�@�$�@��@��T@�x�@�X@��@�Ĝ@���@�(�@��@��P@��@�S�@�C�@�"�@��!@�n�@���@�v�@�J@���@��h@�X@�&�@�V@���@���@��@���@���@�p�@�O�@�?}@�7L@��@���@�bN@��@�t�@�K�@�+@��@���@�~�@�V@�V@�M�@��@��#@���@�7L@�/@�V@��@��D@�z�@�9X@�ƨ@�|�@�dZ@�\)@�S�@�S�@�C�@�o@��y@�ȴ@��R@�ff@�@�@���@��@���@�x�@�&�@��/@��@�bN@� �@��m@���@���@���@��9@���@}�@r~�@h�`@bM�@Y�#@SdZ@L�@D�@<�@7;d@2-@-��@)G�@$��@\)@x�@�
@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˕�A˓uA˓uAˑhA˕�A�I�A�|�A�5?A�"�A��A��A�{A�VA�JA�
=A�%A�  A���A���A���A���A���A���A���A�  A�A�1A�1A�1A�1A�
=A�
=A�%A�A�  A���A���A���A��`A�1Aơ�A�A�t�Ağ�A�"�A�5?A�z�A�  A�l�A���A��+A�Q�A��\A�&�A��uA��A��\A���A���A��A�1'A�  A��7A�G�A��A�G�A��!A��A�dZA�-A�^5A�A�l�A���A�?}A���A�A��A��HA�1'A�$�A��PA�p�A�{A���A��RA�dZA�VA�A�A���A��\A��jA�VA��A���A�A�^5A���A�VA��RA��A�+A���A��A��FA�{A���A�XA�%A� �A�A�A�VA��FA��;A~1Ay�Ay7LAu�^AtE�AsVAq�Ap��Ao"�Am��AlbNAk�PAgS�Ad��Ab��A`��A_��A^A\(�AZ��AY��AX��AV�`AT�HASXAQ��AP��AO��AM��AK`BAI�AHjAG�AF�AE�
AC�;AA�A@-A?��A?hsA>~�A<��A;��A:��A:5?A9l�A7�-A6n�A5�;A3�
A2��A1�A133A/��A/��A/VA.1A-S�A+�;A*�uA(��A'�7A'33A&�yA%��A#�A#��A"{A��AG�A�Av�A�wA{A|�A`BA?}A%A�!A1AhsA��A�A"�A��A�A��A�wA�!A^5A�hA�Ar�AffAA�A33A
VA	�TA	XA�HA��AS�An�A/AI�A�AO�A�A��AJAC�A ĜA {@�o@���@�^5@�V@�x�@�9X@�|�@���@��@�V@�V@�p�@�%@�(�@�n�@�J@�9X@�+@�O�@�I�@�1@�!@� �@���@ߕ�@�@��@ى7@���@�  @ׅ@�;d@�ff@���@�b@�9X@�r�@���@�O�@���@Ѓ@�I�@�(�@� �@�  @��@Ϯ@�t�@�
=@�~�@͑h@˥�@�o@ʸR@�^5@��T@ɡ�@�&�@�C�@���@��@�Ĝ@ģ�@�bN@öF@��@���@�@�p�@�1'@��@�C�@���@�x�@�t�@��!@�V@�X@��@�(�@�"�@�$�@�hs@�&�@���@�(�@�1@�9X@���@��@�O�@�V@�Z@��m@�;d@�5?@�X@���@�r�@��@��@�|�@�t�@�l�@���@�1@�t�@�33@�@�~�@��+@��+@�v�@��@�?}@� �@��;@��F@�t�@�o@�=q@��#@��@�O�@��`@�9X@��m@��P@�C�@��H@��\@�n�@�-@��@���@��7@�&�@���@��@� �@��;@���@�@�~�@�-@��T@��^@���@��7@�hs@�/@��`@��D@�I�@�b@��@��F@��P@�\)@�;d@�@��H@�-@��#@���@�x�@�&�@��`@�Ĝ@��@�I�@��@�+@���@���@�n�@�$�@��@��T@�x�@�X@��@�Ĝ@���@�(�@��@��P@��@�S�@�C�@�"�@��!@�n�@���@�v�@�J@���@��h@�X@�&�@�V@���@���@��@���@���@�p�@�O�@�?}@�7L@��@���@�bN@��@�t�@�K�@�+@��@���@�~�@�V@�V@�M�@��@��#@���@�7L@�/@�V@��@��D@�z�@�9X@�ƨ@�|�@�dZ@�\)@�S�@�S�@�C�@�o@��y@�ȴ@��R@�ff@�@�@���@��@���@�x�@�&�@��/@��@�bN@� �@��m@���@���@���@��9@���@}�@r~�@h�`@bM�@Y�#@SdZ@L�@D�@<�@7;d@2-@-��@)G�@$��@\)@x�@�
@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B�{B�oB�hB�oB�oB�oB�oB�oB�oB�oB�oB�hB�oB�oB�uB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�NBVB�B�B{BDB��B%B+BN�BXB^5BaHBbNB^5BO�BG�B:^B)�B"�B�B�B�BVB��B�fB�
B��B��BŢB�qB�B��B��B�oB�DB�%B�By�Bq�Bk�BZB@�B5?B.B!�BhBB��B�B�B�;B��B�jB��B��B�{B�%Bz�BffBR�BH�B@�B<jB7LB33B%�BoB
��B
�ZB
�5B
�B
ŢB
�B
��B
�B
s�B
ZB
?}B
6FB
 �B
�B
\B
+B
  B	��B	�B	�sB	�HB	��B	�}B	�3B	��B	��B	��B	�PB	�+B	� B	x�B	q�B	k�B	dZB	^5B	YB	S�B	K�B	B�B	;dB	7LB	49B	1'B	+B	!�B	�B	{B	uB	oB	VB	B	B	B��B��B��B�B�B�ZB�BB�5B�#B�B�B��B��B��BȴBĜB�}B�jB�^B�RB�9B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�JB�7B�B�B� B}�B{�By�Bw�Bv�Bs�Bo�Bn�Bn�Bm�Bk�BjBiyBhsBhsBhsBffBdZBaHB`BB_;B^5B^5B]/B\)B[#BZBYBYBXBW
BT�BVBW
BW
BW
BYBYBcTBbNBdZBe`BffBe`Be`Be`BdZBe`BhsBo�Bt�Bu�By�B|�B�B�+B�7B�DB�JB�JB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�LB�LB�?B�9B�RB��BÖBÖBBÖBÖBŢBƨBƨBȴB��B��B��B��B��B��B��B�
B�B�/B�;B�HB�fB�B�B�B�B�B	  B	DB	bB	hB	hB	hB	uB	{B	{B	{B	�B	�B	�B	�B	 �B	 �B	"�B	&�B	(�B	(�B	,B	0!B	33B	49B	33B	5?B	6FB	:^B	=qB	@�B	A�B	C�B	G�B	H�B	J�B	K�B	N�B	O�B	Q�B	T�B	XB	\)B	_;B	bNB	e`B	ffB	iyB	jB	m�B	o�B	q�B	t�B	v�B	x�B	{�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�JB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�?B	�FB	�LB	�RB	�^B	�qB	�qB	�wB	��B	ÖB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�;B	�`B	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
	7B
hB
{B
 �B
+B
0!B
6FB
;dB
@�B
G�B
M�B
R�B
XB
\)B
`BB
cTB
hsB
m�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�tB�jB�qB�tB�vB�tB�tB�vB�tB�tB�jB�tB�pB�zB�xB�xB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�TB[B�B�B~BGB��B'B+BN�BXB^<BaOBbUB^7BO�BG�B:bB)�B"�B�B�B�BXB��B�iB�B��B��BţB�uB�B��B��B�sB�EB�'B�By�Bq�Bk�BZ B@�B5CB.B!�BiBB��B�B�B�<B��B�oB��B��B�|B�+Bz�BfiBR�BH�B@�B<lB7QB36B%�BsB
��B
�_B
�8B
�
B
ŤB
�B
��B
�B
s�B
Z!B
?�B
6QB
 �B
�B
gB
7B
 
B	��B	�B	�B	�RB	��B	��B	�?B	��B	��B	��B	�]B	�8B	�B	x�B	q�B	k�B	diB	^CB	Y)B	TB	K�B	B�B	;vB	7[B	4LB	16B	+B	!�B	�B	�B	�B	�B	iB	,B	/B	B�
B��B��B�B�B�pB�VB�LB�5B�$B�B�B��B��B��BıB��B�B�sB�hB�PB�3B�B� B��B��B��B��B��B��B��B��B��B��B��B�{B�cB�PB�5B�%B�B~
B{�By�Bw�Bv�Bs�Bo�Bn�Bn�Bm�Bk�Bj�Bi�Bh�Bh�Bh�Bf�BdsBadB`[B_UB^QB^NB]IB\EB[=BZ6BY3BY0BX(BW#BUBVBW%BW&BW$BY2BY0BcnBbiBdsBezBf�BeyBeyBeyBdsBexBh�Bo�Bt�Bu�By�B}B�B�CB�NB�ZB�aB�`B�kB�vB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�(B�0B�CB�NB�aB�`B�WB�MB�hB��BéBêB£BêBìBŷBƽBƼB��B��B��B��B��B�	B�B�B�B�*B�AB�NB�ZB�yB�B�B��B�B��B	 B	UB	sB	zB	yB	yB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	&�B	)B	)B	,B	0/B	3CB	4FB	3DB	5NB	6TB	:mB	=�B	@�B	A�B	C�B	G�B	H�B	J�B	K�B	N�B	O�B	Q�B	UB	XB	\6B	_IB	b\B	emB	ftB	i�B	j�B	m�B	o�B	q�B	t�B	v�B	x�B	{�B	�B	�$B	�0B	�2B	�7B	�BB	�KB	�YB	�eB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�2B	�9B	�IB	�OB	�ZB	�_B	�iB	�}B	�{B	��B	��B	âB	ǼB	ǻB	ȾB	ȾB	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�!B	� B	�,B	�CB	�hB	�hB	�iB	�rB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
B
B
B
B
#B
.B
	@B
qB
�B
 �B
+B
0)B
6MB
;lB
@�B
G�B
M�B
R�B
XB
\0B
`KB
cZB
hxB
m�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230508    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230508  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230508  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                