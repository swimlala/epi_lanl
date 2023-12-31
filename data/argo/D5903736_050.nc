CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:32Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230532  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               2A   AO  4051_7090_050                   2C  D   APEX                            5368                            041511                          846 @ֱ�	�` 1   @ֱ���_�@4�~��"��e��E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    2A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBN��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D��D�@ D�y�D�ɚD��D�P D��fD��fD���D�FfD�i�D��fD��D�L�DچfD�� D�  D�@ D�ffD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BHQ�BN�RBW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C2{C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt޸Dy��D�)D�?\D�x�D���D��D�O\D���D���D��)D�E�D�h�D���D�)D�L)Dڅ�D��\D��\D�?\D�e�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��#A��A��A��A��/A��TA��A��yA��A��A��A���A���A۟�Aە�Aۏ\Aۏ\Aۏ\A�l�A�G�A�;dA�Q�A׬A�ĜA�S�A�=qA��yA��mAġ�A�r�A��TA�?}A���A�bA�;dA��A��TA�bNA��A��A��RA�I�A�p�A���A��^A�l�A�K�A��A��A��yA�ZA��TA� �A���A��
A��A�ffA�A���A�E�A�5?A�`BA��A�ȴA�%A�-A���A��FA�33A�oA�+A��9A�hsA��;A�1'A���A�A�A�x�A�ƨA��FA���A���A�O�A���A���A�(�A���A�G�A�z�A�(�A�I�A�%A�r�A|�A~~�A{33Ax�HAx��AxVAu��Aq�Al��Ak"�Aj5?Ag�
Af�`Ae|�Ae7LAd�Ad^5Ac�wA`�HA\�uAZr�AW��AV�uAU�7AT�!AShsARz�APA�AOAM�PAL�AK�AJ�AHz�AG�wAG
=AF��AFM�AF(�AD�AA�
A@v�A=x�A<VA;�-A;7LA:JA8�uA7�TA6r�A4�\A1?}A/ƨA/"�A.��A.�RA.v�A-��A,v�A)O�A'��A'"�A&�jA%\)A$(�A#
=A"Q�A"9XA"(�A" �A"�A"  A!oA 1'A�mA��A\)AffA�A+A��A1A�AĜA�wA��A{A�At�A;dA��AXA�^A-Al�A�!A^5A$�A?}A�AA$�A��A	�hAr�A�A�AK�A%A�jA�FA��A�A b@���@���@�?}@�(�@��y@��\@��h@�  @�ff@��@�  @�S�@���@�5?@��@�\@�bN@�
=@��H@�$�@��u@�t�@���@�9X@ج@�hs@�(�@ӶF@�\)@��@��@҇+@��T@�%@д9@�(�@�\)@�z�@ʇ+@ə�@ț�@ư!@��@�(�@�dZ@¸R@�x�@��u@���@�t�@���@���@�Ĝ@�ƨ@�l�@�ȴ@���@�n�@��^@�O�@��@�A�@�+@���@��/@��
@���@�n�@��h@�V@��/@��j@��D@�A�@�9X@�(�@�b@��
@��P@�\)@�+@��@��R@��!@�~�@�^5@�V@�$�@���@���@�/@��j@��u@�b@���@�&�@��u@�9X@�1@�b@�ƨ@�l�@�K�@���@��@���@��h@�p�@�`B@�/@��/@���@���@�A�@��
@�\)@��@���@�^5@�5?@�5?@�5?@�-@���@�%@��/@�Ĝ@�z�@�j@�Q�@�(�@�1@��
@��@�J@��!@�-@�j@��@�b@��@��w@�t�@�K�@��@�"�@�o@��@��@���@���@�$�@��@��T@���@�hs@�X@�V@�Q�@�I�@�9X@� �@�  @��m@�ƨ@��@���@��@�S�@�o@��y@���@���@���@���@���@���@�~�@�ff@�V@�=q@�$�@��@���@�G�@�&�@��9@���@�|�@��P@���@��F@��@��@��@�|�@��@�l�@�"�@�
=@�+@�"�@��@�o@�@��@�5?@�{@�@��^@�hs@�G�@�&�@���@��/@�Ĝ@���@���@��u@��@�;d@�
=@�v�@�@��h@�x�@�7L@���@���@�b@��
@��@��P@�33@�o@��@��@��@��y@���@�v�@�=q@��@�@�@�hs@��@��@��D@�bN@�Z@� �@���@�33@�33@���@�ȴ@��R@��!@��!@���@��\@�^5@�V@��@��-@��h@�?}@���@��`@��/@��/@��/@���@���@�1'@���@�dZ@�@���@�%@zn�@s33@jJ@`bN@W��@O�w@L1@G+@A7L@;��@3��@-O�@&ȴ@ ��@��@�R@@�P@V@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��#A��A��A��A��/A��TA��A��yA��A��A��A���A���A۟�Aە�Aۏ\Aۏ\Aۏ\A�l�A�G�A�;dA�Q�A׬A�ĜA�S�A�=qA��yA��mAġ�A�r�A��TA�?}A���A�bA�;dA��A��TA�bNA��A��A��RA�I�A�p�A���A��^A�l�A�K�A��A��A��yA�ZA��TA� �A���A��
A��A�ffA�A���A�E�A�5?A�`BA��A�ȴA�%A�-A���A��FA�33A�oA�+A��9A�hsA��;A�1'A���A�A�A�x�A�ƨA��FA���A���A�O�A���A���A�(�A���A�G�A�z�A�(�A�I�A�%A�r�A|�A~~�A{33Ax�HAx��AxVAu��Aq�Al��Ak"�Aj5?Ag�
Af�`Ae|�Ae7LAd�Ad^5Ac�wA`�HA\�uAZr�AW��AV�uAU�7AT�!AShsARz�APA�AOAM�PAL�AK�AJ�AHz�AG�wAG
=AF��AFM�AF(�AD�AA�
A@v�A=x�A<VA;�-A;7LA:JA8�uA7�TA6r�A4�\A1?}A/ƨA/"�A.��A.�RA.v�A-��A,v�A)O�A'��A'"�A&�jA%\)A$(�A#
=A"Q�A"9XA"(�A" �A"�A"  A!oA 1'A�mA��A\)AffA�A+A��A1A�AĜA�wA��A{A�At�A;dA��AXA�^A-Al�A�!A^5A$�A?}A�AA$�A��A	�hAr�A�A�AK�A%A�jA�FA��A�A b@���@���@�?}@�(�@��y@��\@��h@�  @�ff@��@�  @�S�@���@�5?@��@�\@�bN@�
=@��H@�$�@��u@�t�@���@�9X@ج@�hs@�(�@ӶF@�\)@��@��@҇+@��T@�%@д9@�(�@�\)@�z�@ʇ+@ə�@ț�@ư!@��@�(�@�dZ@¸R@�x�@��u@���@�t�@���@���@�Ĝ@�ƨ@�l�@�ȴ@���@�n�@��^@�O�@��@�A�@�+@���@��/@��
@���@�n�@��h@�V@��/@��j@��D@�A�@�9X@�(�@�b@��
@��P@�\)@�+@��@��R@��!@�~�@�^5@�V@�$�@���@���@�/@��j@��u@�b@���@�&�@��u@�9X@�1@�b@�ƨ@�l�@�K�@���@��@���@��h@�p�@�`B@�/@��/@���@���@�A�@��
@�\)@��@���@�^5@�5?@�5?@�5?@�-@���@�%@��/@�Ĝ@�z�@�j@�Q�@�(�@�1@��
@��@�J@��!@�-@�j@��@�b@��@��w@�t�@�K�@��@�"�@�o@��@��@���@���@�$�@��@��T@���@�hs@�X@�V@�Q�@�I�@�9X@� �@�  @��m@�ƨ@��@���@��@�S�@�o@��y@���@���@���@���@���@���@�~�@�ff@�V@�=q@�$�@��@���@�G�@�&�@��9@���@�|�@��P@���@��F@��@��@��@�|�@��@�l�@�"�@�
=@�+@�"�@��@�o@�@��@�5?@�{@�@��^@�hs@�G�@�&�@���@��/@�Ĝ@���@���@��u@��@�;d@�
=@�v�@�@��h@�x�@�7L@���@���@�b@��
@��@��P@�33@�o@��@��@��@��y@���@�v�@�=q@��@�@�@�hs@��@��@��D@�bN@�Z@� �@���@�33@�33@���@�ȴ@��R@��!@��!@���@��\@�^5@�V@��@��-@��h@�?}@���@��`@��/@��/@��/@���@���@�1'@���@�dZ@�@���@�%@zn�@s33@jJ@`bN@W��@O�w@L1@G+@A7L@;��@3��@-O�@&ȴ@ ��@��@�R@@�P@V@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�!B}�Bp�Bp�Bl�BdZB^5B_;B`BBYBT�BZBYBXBXBW
BT�BQ�BL�BI�BC�B>wB<jB:^B7LB1'B �B{BB��B�B�B�BB�B��B��B�XB��B�{B�Bm�B^5BL�B2-B"�B�B%B�B�HB�#B��BƨB�^B�B�+B\)B=qB.B�B
��B
�B
��B
ĜB
�dB
��B
��B
�uB
x�B
ZB
Q�B
F�B
>wB
,B
�B
�B
�B
B	�yB	��B	��B	ƨB	�^B	�?B	�B	�B	�B	��B	��B	�\B	}�B	s�B	jB	e`B	`BB	[#B	T�B	O�B	F�B	>wB	6FB	.B	,B	%�B	�B	�B	�B	�B	uB	hB	
=B	  B��B��B�B�B�sB�TB�5B�B��B��BǮBÖB��B��B�}B�wB�^B�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�JB�DB�7B�+B�B�B�B�B� B~�B|�Bx�Bu�Bs�Br�Bq�Bp�Bo�Bm�Bk�BiyBgmBcTBaHBaHB`BB`BB_;B^5B]/B]/B[#B[#BZBXBZBZB[#B\)B\)B\)B]/B^5B_;B`BB`BB_;BaHBcTBbNBcTBe`BffBiyBiyBjBhsBffBm�Bp�Bs�Bu�Bv�Bv�Bw�Bw�By�Bz�Bz�B{�Bz�B{�B~�B� B�B�%B�7B�DB�VB�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�3B�FB�LB�^B�wBĜBɺB��B��B�
B�)B�5B�BB�HB�TB�fB�sB�yB�yB�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	%B	B	B	  B	B	DB	{B	�B	�B	�B	�B	�B	"�B	&�B	'�B	'�B	'�B	'�B	)�B	+B	+B	,B	0!B	5?B	8RB	:^B	=qB	>wB	?}B	?}B	?}B	?}B	?}B	C�B	D�B	D�B	G�B	G�B	I�B	K�B	L�B	L�B	J�B	J�B	O�B	R�B	Q�B	S�B	T�B	W
B	XB	[#B	_;B	dZB	ffB	hsB	l�B	o�B	s�B	t�B	w�B	w�B	y�B	{�B	|�B	|�B	~�B	�B	�B	�%B	�%B	�+B	�1B	�1B	�7B	�7B	�7B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�FB	�LB	�XB	�jB	�jB	�}B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�TB	�ZB	�`B	�`B	�`B	�ZB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
1B
uB
�B
 �B
+B
5?B
<jB
@�B
D�B
H�B
L�B
T�B
ZB
aHB
ffB
jB
o�B
r�B
v�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�)B}�Bp�Bp�Bl�Bd_B^9B_>B`HBYBUBZ#BYBXBXBWBUBQ�BL�BI�BC�B>|B<qB:bB7SB1-B �B|B#B��B�B�B�HB�!B�B��B�\B��B�}B�Bm�B^5BL�B21B"�B�B'B�B�LB�&B��BƩB�aB�B�-B\,B=uB.B�B
��B
� B
��B
ġB
�hB
��B
��B
�zB
x�B
Z#B
Q�B
F�B
>B
,B
�B
�B
�B
%B	�B	�B	��B	ƵB	�jB	�HB	�(B	�B	�B	��B	��B	�hB	~B	s�B	j�B	epB	`QB	[1B	UB	O�B	F�B	>�B	6WB	.%B	,B	%�B	�B	�B	�B	�B	�B	}B	
QB	 B�B��B�B�B�B�iB�KB�1B�B��B��BíB��B��B��B��B�rB�PB�#B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�sB�iB�aB�]B�OB�CB�4B�+B�%B�B�BB}Bx�Bu�Bs�Br�Bq�Bp�Bo�Bm�Bk�Bi�Bg�BcoBabBaaB`]B`\B_TB^PB]HB]HB[>B[<BZ5BX+BZ8BZ3B[<B\BB\BB\AB]GB^PB_QB`]B`]B_TBabBclBbhBclBezBfBi�Bi�Bj�Bh�Bf~Bm�Bp�Bs�Bu�Bv�Bv�Bw�Bw�By�Bz�Bz�B{�Bz�B{�BB�B�B�=B�NB�YB�nB�uB��B��B��B��B��B��B��B��B��B��B�+B�0B�HB�[B�bB�qB��BıB��B��B�B�B�<B�GB�SB�[B�jB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B�B	 B	+B	5B	6B	1B	"B	 B	0B	VB	�B	�B	�B	�B	�B	�B	"�B	&�B	'�B	( B	(B	'�B	*B	+B	+B	,B	0.B	5PB	8_B	:pB	=�B	>�B	?�B	?�B	?�B	?�B	?�B	C�B	D�B	D�B	G�B	G�B	I�B	K�B	L�B	L�B	J�B	J�B	O�B	SB	Q�B	TB	UB	WB	XB	[1B	_JB	dgB	fsB	h�B	l�B	o�B	s�B	t�B	w�B	w�B	y�B	{�B	|�B	|�B	B	�*B	�,B	�2B	�0B	�7B	�<B	�=B	�EB	�DB	�DB	�VB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�-B	�=B	�IB	�QB	�VB	�fB	�sB	�uB	��B	B	ħB	ǹB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�'B	�%B	�.B	�/B	�2B	�@B	�]B	�dB	�iB	�jB	�jB	�dB	�bB	�cB	�^B	�^B	�^B	�dB	�iB	�pB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B
 
B
B
B
B
B
*B
:B
B
�B
 �B
+B
5FB
<qB
@�B
D�B
H�B
L�B
UB
Z%B
aPB
fkB
j�B
o�B
r�B
v�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230532  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                