CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:29Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230529  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  4051_7090_044                   2C  D   APEX                            5368                            041511                          846 @֢:�� 1   @֢;J� 
@5%�����d��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   A   A   @�  @�  A��A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�fD�@ D���D���D�fD�FfD��3D�� D�3D�33D�,�D�ٚD�fD�<�Dڙ�D��3D���D�0 D�vfD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)AG�A�A?�A^zA�A��
A��
A��
A��
A��
A��
A��
A��
B�BQ�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Ct{Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt�RDy�RD��D�?\D��)D���D��D�E�D���D��\D��D�2�D�,)D���D��D�<)Dژ�D�D��)D�/\D�u�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�/A�-A�/A�1'A��A��A�A���A���A���A��A��A��A��yA��HA��#A���A�|�A�%A���A��A��A�z�A�I�A�;dA�ȴA���A���A��uA�I�A�\)A��A�;dA�5?A��A�I�A�A�t�A�"�A��^A�G�A���A�jA�x�A��RA��^A�G�A��A��yA�"�A��A�M�A���A�Q�A���A�ƨA�JA��A�ffA�A�z�A��A��A��jA�=qA�|�A�&�A��7A�VA�+A��wA�ffA��mA���A�(�A��A���A�  A��jA��A��A��#A��\A�ƨA�/A��RA��A��A�%A�{A��`A�bNA��FA�%A���A���A�(�A��9A�\)A��/A�bA�5?A�l�A��yA��A���A��-A�ƨA�A��A�ĜA��A���A/A}O�Ay�;Av�\Au?}As;dAqp�ApJAl �AjZAh�`Ae33Ac�Ac�AbI�AaS�A`�A`bA_?}A]oA[�
AY�hAX �AW�-AWXAVbNATA�AS/ARQ�AQXAO�^ANffAMp�ALAJ�AG�mAF��AE+ADv�AC�hAB  A@JA?��A>�!A=�mA=�FA=�A<�!A<=qA;dZA9O�A8��A8=qA7��A6(�A4��A4n�A3"�A0�A0bA/|�A-�TA-XA,��A+�^A+"�A*E�A)C�A(ZA'��A%��A$�DA"�A"M�A!A ȴA��A�A�!A��AS�AffA��A\)A�A1A+A��AĜA�TAE�A33A�DA{A?}A��A+A�
A33A�yA�!AE�A
A�AVA��A��A|�A��A�A?}A�uA�A��AhsA �@��\@�hs@���@�1@�\)@�^5@��@���@�7L@��@�@��@�bN@�R@�D@�dZ@�$�@���@���@�7@�%@���@�z�@���@睲@�@�C�@�ȴ@�O�@�A�@���@�@�p�@�j@�C�@݁@ܛ�@�E�@���@�Z@��@ׅ@�x�@���@�o@җ�@���@ѡ�@�7L@��`@мj@�9X@�l�@���@�t�@��@���@ʗ�@ɑh@�7L@�(�@�J@ũ�@�p�@�?}@�/@ģ�@�l�@�=q@��7@�j@��@��m@��@�V@���@�?}@��@��
@�dZ@�ȴ@��#@���@�o@�p�@�1'@��@�ȴ@��@�`B@���@�ƨ@�33@�ȴ@�5?@��@�p�@���@�I�@� �@���@��F@��@�v�@�-@��7@�&�@���@�z�@�  @�ƨ@��w@���@�l�@�@���@��T@�/@��m@���@�33@�ȴ@�ff@��@���@��@��/@�bN@�l�@�o@��R@�n�@�@���@�p�@�X@���@�j@�1@��m@��m@��
@���@��@��@���@�9X@���@���@�n�@�5?@�J@��T@��@�Z@�hs@��@���@���@�r�@��F@���@���@�C�@�Z@��7@�X@���@��@�"�@�S�@���@��@�{@��@���@��/@��@�1'@��@��7@�p�@�%@���@���@��@�V@��/@��D@�b@��@�ff@�{@��@���@��-@��h@�`B@�G�@�G�@�G�@�&�@��j@�I�@�  @��w@���@�t�@�;d@��@���@�X@�/@���@��@�1'@��@���@�\)@�C�@��@��y@���@��\@�$�@��#@���@�`B@�X@�G�@�G�@�G�@���@��j@�z�@�I�@�  @��;@��@���@��P@��@�t�@�dZ@�"�@���@��H@��R@���@�~�@�=q@��@�{@�{@�@�@��@���@��@�%@��
@��@}��@t�@l9X@`��@Y��@S@J�\@B�H@=��@7�;@0�9@)x�@#�
@@�@�T@n�@�P@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�/A�-A�/A�1'A��A��A�A���A���A���A��A��A��A��yA��HA��#A���A�|�A�%A���A��A��A�z�A�I�A�;dA�ȴA���A���A��uA�I�A�\)A��A�;dA�5?A��A�I�A�A�t�A�"�A��^A�G�A���A�jA�x�A��RA��^A�G�A��A��yA�"�A��A�M�A���A�Q�A���A�ƨA�JA��A�ffA�A�z�A��A��A��jA�=qA�|�A�&�A��7A�VA�+A��wA�ffA��mA���A�(�A��A���A�  A��jA��A��A��#A��\A�ƨA�/A��RA��A��A�%A�{A��`A�bNA��FA�%A���A���A�(�A��9A�\)A��/A�bA�5?A�l�A��yA��A���A��-A�ƨA�A��A�ĜA��A���A/A}O�Ay�;Av�\Au?}As;dAqp�ApJAl �AjZAh�`Ae33Ac�Ac�AbI�AaS�A`�A`bA_?}A]oA[�
AY�hAX �AW�-AWXAVbNATA�AS/ARQ�AQXAO�^ANffAMp�ALAJ�AG�mAF��AE+ADv�AC�hAB  A@JA?��A>�!A=�mA=�FA=�A<�!A<=qA;dZA9O�A8��A8=qA7��A6(�A4��A4n�A3"�A0�A0bA/|�A-�TA-XA,��A+�^A+"�A*E�A)C�A(ZA'��A%��A$�DA"�A"M�A!A ȴA��A�A�!A��AS�AffA��A\)A�A1A+A��AĜA�TAE�A33A�DA{A?}A��A+A�
A33A�yA�!AE�A
A�AVA��A��A|�A��A�A?}A�uA�A��AhsA �@��\@�hs@���@�1@�\)@�^5@��@���@�7L@��@�@��@�bN@�R@�D@�dZ@�$�@���@���@�7@�%@���@�z�@���@睲@�@�C�@�ȴ@�O�@�A�@���@�@�p�@�j@�C�@݁@ܛ�@�E�@���@�Z@��@ׅ@�x�@���@�o@җ�@���@ѡ�@�7L@��`@мj@�9X@�l�@���@�t�@��@���@ʗ�@ɑh@�7L@�(�@�J@ũ�@�p�@�?}@�/@ģ�@�l�@�=q@��7@�j@��@��m@��@�V@���@�?}@��@��
@�dZ@�ȴ@��#@���@�o@�p�@�1'@��@�ȴ@��@�`B@���@�ƨ@�33@�ȴ@�5?@��@�p�@���@�I�@� �@���@��F@��@�v�@�-@��7@�&�@���@�z�@�  @�ƨ@��w@���@�l�@�@���@��T@�/@��m@���@�33@�ȴ@�ff@��@���@��@��/@�bN@�l�@�o@��R@�n�@�@���@�p�@�X@���@�j@�1@��m@��m@��
@���@��@��@���@�9X@���@���@�n�@�5?@�J@��T@��@�Z@�hs@��@���@���@�r�@��F@���@���@�C�@�Z@��7@�X@���@��@�"�@�S�@���@��@�{@��@���@��/@��@�1'@��@��7@�p�@�%@���@���@��@�V@��/@��D@�b@��@�ff@�{@��@���@��-@��h@�`B@�G�@�G�@�G�@�&�@��j@�I�@�  @��w@���@�t�@�;d@��@���@�X@�/@���@��@�1'@��@���@�\)@�C�@��@��y@���@��\@�$�@��#@���@�`B@�X@�G�@�G�@�G�@���@��j@�z�@�I�@�  @��;@��@���@��P@��@�t�@�dZ@�"�@���@��H@��R@���@�~�@�=q@��@�{@�{@�@�@��@���@��@�%@��
@��@}��@t�@l9X@`��@Y��@S@J�\@B�H@=��@7�;@0�9@)x�@#�
@@�@�T@n�@�P@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B�B�B��B�ZB�B�B�yBɺB�dB�FB�?B�B�wB��B�jB�dB�FB�B��B�uB�+B� Bz�Bw�Bu�Bq�Bl�Be`B^5BXBP�BL�BE�B8RB1'B)�B&�B!�B�BuB
=BB��B��B��B�B�mB�TB�;B��BƨB��B�RB�'B��B��B��B��B�hB�7Bv�Be`BXBR�BC�B9XB49B-B�B�BJB��B�B��B�^B��B��B�hBt�BQ�B@�B7LB.B�BB
��B
�B
�#B
ȴB
�dB
�-B
��B
��B
�\B
�=B
�B
t�B
cTB
VB
I�B
5?B
!�B
�B
DB
B	��B	�NB	�B	��B	�XB	�!B	�B	��B	��B	��B	��B	��B	�JB	�B	x�B	p�B	m�B	k�B	gmB	`BB	\)B	W
B	Q�B	I�B	E�B	A�B	=qB	5?B	)�B	%�B	!�B	�B	�B	JB	B	B��B��B��B��B��B��B��B�B�B�B�yB�TB�HB�HB�)B��B��BɺBȴBɺBɺBŢBB�}B�dB�XB�FB�'B�B��B��B��B��B��B��B��B�{B�oB�bB�\B�PB�DB�1B�B�B}�By�Bu�Br�Bp�Bn�Bl�BiyBffBdZBcTBaHB`BB\)BT�BP�BP�BO�BN�BM�BM�BO�BP�BP�BP�BN�BH�BE�BD�BB�BA�B@�B>wB<jB9XB7LB8RB7LB6FB5?B5?B6FB8RB:^B<jB<jB?}B@�B?}B@�B@�B@�B@�B@�BA�BM�BW
B]/B`BB`BB_;B`BBbNBe`Bk�Bm�Bn�Bo�Bn�Bw�B|�B~�B� B�B�B�B�B�B�B�%B�VB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�3B�?B�LB�dB�qB�}B�}B��BĜBƨBɺB��B��B��B�B�BB�TB�mB�B�B�B�B�B��B��B��B��B	  B	B	B	+B	1B		7B	
=B	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	'�B	(�B	(�B	(�B	.B	0!B	1'B	1'B	2-B	49B	6FB	8RB	:^B	:^B	<jB	G�B	M�B	P�B	Q�B	T�B	W
B	W
B	W
B	W
B	XB	XB	\)B	jB	n�B	n�B	s�B	w�B	w�B	z�B	}�B	� B	�1B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�?B	�dB	�dB	�^B	�jB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
VB
�B
�B
(�B
33B
9XB
=qB
B�B
H�B
N�B
S�B
YB
^5B
bNB
dZB
hsB
k�B
o�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�fB�B�B�B��B�lB�PB�HB�B��B��B�rB�jB�KB�B��B�|B�3B�Bz�Bw�Bu�Bq�Bl�BedB^:BXBP�BL�BE�B8[B1+B*B&�B!�B�ByB
BBB� B��B��B�B�sB�ZB�;B�BƬB��B�VB�*B��B��B��B��B�kB�6Bv�Be`BXBR�BC�B9[B4:B-B�B�BMB��B�B��B�`B��B��B�iBt�BQ�B@�B7NB.B�B
B
��B
�B
�'B
ȸB
�gB
�1B
��B
��B
�cB
�DB
�B
t�B
c^B
VB
I�B
5IB
!�B
�B
MB
B	��B	�WB	�B	��B	�dB	�.B	�B	�B	��B	��B	��B	��B	�XB	�!B	x�B	p�B	m�B	k�B	g}B	`QB	\7B	WB	Q�B	I�B	E�B	A�B	=�B	5PB	*B	%�B	!�B	�B	�B	\B	0B	B�B�B�B��B��B��B��B�B�B�B�B�iB�[B�]B�=B��B��B��B��B��B��BŷB£B��B�{B�lB�\B�;B�"B�B��B��B��B��B��B��B��B��B�yB�sB�gB�]B�HB�7B�B~By�Bu�Br�Bp�Bn�Bl�Bi�Bf�BdsBckBabB`^B\BBUBQ BQ BO�BN�BM�BM�BO�BP�BQ BQ BN�BH�BE�BD�BB�BA�B@�B>�B<�B9tB7hB8mB7hB6`B5\B5ZB6aB8lB:yB<�B<�B?�B@�B?�B@�B@�B@�B@�B@�BA�BM�BW"B]EB`[B`]B_RB`YBbdBezBk�Bm�Bn�Bo�Bn�Bw�B}BB�B�B� B�+B�'B�-B�6B�:B�kB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�+B�/B�<B�CB�IB�TB�bB�yB��B��B��B��BĳBƼB��B��B��B� B�0B�TB�eB�B�B�B�B�B��B��B��B��B�B	 B	B	2B	<B	AB		FB	
NB	[B	fB	yB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	'�B	)B	)B	)B	.&B	02B	19B	18B	2<B	4IB	6UB	8aB	:mB	:mB	<xB	G�B	M�B	P�B	Q�B	U
B	WB	WB	WB	WB	XB	XB	\6B	j�B	n�B	n�B	s�B	w�B	w�B	z�B	~B	�B	�;B	�{B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�(B	�B	��B	�B	�IB	�pB	�nB	�jB	�wB	��B	��B	áB	àB	ĩB	ĨB	ħB	ĨB	ŭB	ŬB	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	� B	�)B	�'B	�%B	�!B	�B	�B	�B	�'B	�2B	�8B	�AB	�JB	�QB	�SB	�VB	�]B	�^B	�cB	�hB	�pB	�xB	�vB	�|B	�~B	�~B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
)B
`B
�B
�B
(�B
3=B
9^B
=xB
B�B
H�B
N�B
S�B
YB
^=B
bWB
daB
hyB
k�B
o�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230529    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230529  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230529  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                