CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:19Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230519  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_025                   2C  D   APEX                            5368                            041511                          846 @�q�n��1   @�q��[��@3���+�d��^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�3D�9�D���D���D�fD�33D�|�D��3D�3D�P D��3D�ٚD��D�33Dڀ D�� D�	�D�9�D�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C�GC	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DIDI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc�RDd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt�RDy��D��D�8�D��)D��)D��D�2�D�|)D�D��D�O\D���D���D�)D�2�D�\D��\D��D�8�D�x�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�A�ȴAϴ9Aϰ!A���A��
A��#A��#A��;A��TA��TA��HA��TA���A���A�A�%A�1A�JA��A��A�"�A�(�A�-A�1'A�33A�7LA�G�A�O�A�S�A�K�A� �A϶FA��`A���A���A�C�A��AƁA�%A��/A���A�z�A��A�JA���A�v�A�bA���A��A��A�C�A��DA���A�A���A��A��A�=qA��A�=qA���A���A�E�A�-A��
A��!A��A���A���A��A�JA�ȴA��A���A��mA���A��A�E�A�l�A���A��A���A��A��jA�7LA���A���A��A��`A�O�A�\)A��A���A���A�A�A��hA���A�(�A�+A��uA�1'A�z�A���A���A�"�A~1A|�HA{x�Az��Ay�;AxbAu�
Ar�/An9XAhv�AfJAd9XAcx�AbE�A]�wA\A�AZ�AY��AX�RAV��AU�mAU"�AT��AS+AR�RAQ�AO�#ANVAN{AM|�AL��AKAJ�/AJ�DAI�AH�`AH9XAFA�AEhsAD�HAD��AC�AB5?AA+A>I�A=+A<bA9��A9K�A9�A8��A8$�A7��A7��A6�A4��A2��A0��A0(�A/`BA.��A-p�A,$�A*1'A)?}A)oA(�\A'\)A&9XA$�A#�A"{A!�A�hA��AVAAA��A1A7LAQ�Ap�A%A��Az�AA�A�#A5?A�A�A�`A�#A
��A
VA	�wA��AbNA�A;dA��Az�A�mAK�AoA�A~�A  A��A&�A1'A�^A ��@���@�O�@��F@�@�A�@�@��/@���@�?}@���@�D@�@�^@��`@�bN@�@旍@�r�@���@�!@�=q@�`B@ߥ�@��#@�p�@�r�@�ƨ@��@�J@ى7@�Ĝ@�9X@�t�@֟�@��#@��`@�(�@ӕ�@�@�~�@�M�@�`B@мj@ЋD@�(�@υ@�@ΰ!@�-@�&�@̋D@�t�@�$�@�p�@�%@�Ĝ@ǅ@���@�~�@�E�@���@ÍP@��@�^5@�x�@�I�@��F@�^5@��@��u@�1'@���@���@�l�@��@���@���@�7L@��@���@�+@�~�@���@�j@�=q@�`B@��7@�`B@��`@���@��@�Q�@���@���@�o@�
=@�^5@���@�7L@�7L@��@��@�=q@��@��#@�^5@�M�@�-@�J@��@���@�/@�V@�r�@��@���@���@��@��@�{@��^@�7L@�V@��/@���@�Ĝ@��9@���@�  @��y@��+@�$�@�{@��h@�7L@�/@�&�@��D@�1@��F@�33@�ȴ@���@���@�n�@��@��7@�&�@��/@���@�A�@���@�|�@�
=@��R@��\@�n�@�5?@�J@��#@��-@�hs@�/@�V@���@���@�Ĝ@���@�I�@�(�@�  @�ƨ@�t�@�33@��@��@��+@�V@�=q@�5?@�$�@�{@��@��7@�V@��9@�j@�9X@��@��w@��P@�\)@�"�@��@��!@��+@��@��@��-@�x�@�7L@��@��/@�r�@�A�@�b@�1@��;@�|�@���@��\@�~�@�^5@�$�@�@��#@��^@���@�`B@��@��9@��9@��@���@�bN@�1'@�b@��m@�ƨ@�l�@���@���@���@���@�v�@��@�@��-@��7@�p�@�?}@�/@��@�%@�Ĝ@��u@�Q�@� �@�b@��m@�ƨ@��w@��w@�t�@�+@���@���@�v�@�ff@�M�@�-@��@�{@�J@���@���@��h@�p�@�`B@�|�@{t�@s��@l��@b=q@YX@P�9@H�9@A�#@<I�@4�/@-�T@'�P@#C�@�@�@l�@"�@ȴ@
�!@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�A�ȴAϴ9Aϰ!A���A��
A��#A��#A��;A��TA��TA��HA��TA���A���A�A�%A�1A�JA��A��A�"�A�(�A�-A�1'A�33A�7LA�G�A�O�A�S�A�K�A� �A϶FA��`A���A���A�C�A��AƁA�%A��/A���A�z�A��A�JA���A�v�A�bA���A��A��A�C�A��DA���A�A���A��A��A�=qA��A�=qA���A���A�E�A�-A��
A��!A��A���A���A��A�JA�ȴA��A���A��mA���A��A�E�A�l�A���A��A���A��A��jA�7LA���A���A��A��`A�O�A�\)A��A���A���A�A�A��hA���A�(�A�+A��uA�1'A�z�A���A���A�"�A~1A|�HA{x�Az��Ay�;AxbAu�
Ar�/An9XAhv�AfJAd9XAcx�AbE�A]�wA\A�AZ�AY��AX�RAV��AU�mAU"�AT��AS+AR�RAQ�AO�#ANVAN{AM|�AL��AKAJ�/AJ�DAI�AH�`AH9XAFA�AEhsAD�HAD��AC�AB5?AA+A>I�A=+A<bA9��A9K�A9�A8��A8$�A7��A7��A6�A4��A2��A0��A0(�A/`BA.��A-p�A,$�A*1'A)?}A)oA(�\A'\)A&9XA$�A#�A"{A!�A�hA��AVAAA��A1A7LAQ�Ap�A%A��Az�AA�A�#A5?A�A�A�`A�#A
��A
VA	�wA��AbNA�A;dA��Az�A�mAK�AoA�A~�A  A��A&�A1'A�^A ��@���@�O�@��F@�@�A�@�@��/@���@�?}@���@�D@�@�^@��`@�bN@�@旍@�r�@���@�!@�=q@�`B@ߥ�@��#@�p�@�r�@�ƨ@��@�J@ى7@�Ĝ@�9X@�t�@֟�@��#@��`@�(�@ӕ�@�@�~�@�M�@�`B@мj@ЋD@�(�@υ@�@ΰ!@�-@�&�@̋D@�t�@�$�@�p�@�%@�Ĝ@ǅ@���@�~�@�E�@���@ÍP@��@�^5@�x�@�I�@��F@�^5@��@��u@�1'@���@���@�l�@��@���@���@�7L@��@���@�+@�~�@���@�j@�=q@�`B@��7@�`B@��`@���@��@�Q�@���@���@�o@�
=@�^5@���@�7L@�7L@��@��@�=q@��@��#@�^5@�M�@�-@�J@��@���@�/@�V@�r�@��@���@���@��@��@�{@��^@�7L@�V@��/@���@�Ĝ@��9@���@�  @��y@��+@�$�@�{@��h@�7L@�/@�&�@��D@�1@��F@�33@�ȴ@���@���@�n�@��@��7@�&�@��/@���@�A�@���@�|�@�
=@��R@��\@�n�@�5?@�J@��#@��-@�hs@�/@�V@���@���@�Ĝ@���@�I�@�(�@�  @�ƨ@�t�@�33@��@��@��+@�V@�=q@�5?@�$�@�{@��@��7@�V@��9@�j@�9X@��@��w@��P@�\)@�"�@��@��!@��+@��@��@��-@�x�@�7L@��@��/@�r�@�A�@�b@�1@��;@�|�@���@��\@�~�@�^5@�$�@�@��#@��^@���@�`B@��@��9@��9@��@���@�bN@�1'@�b@��m@�ƨ@�l�@���@���@���@���@�v�@��@�@��-@��7@�p�@�?}@�/@��@�%@�Ĝ@��u@�Q�@� �@�b@��m@�ƨ@��w@��w@�t�@�+@���@���@�v�@�ff@�M�@�-@��@�{@�J@���@���@��h@�p�@�`B@�|�@{t�@s��@l��@b=q@YX@P�9@H�9@A�#@<I�@4�/@-�T@'�P@#C�@�@�@l�@"�@ȴ@
�!@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBl�BjBjBm�Bq�Bx�B�B�%B�=B�JB�PB�VB�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�XB�qB��B��B��B�wB�RB�!B��B�{B~�Bq�BjBS�BI�BD�BA�B?}B<jB5?B0!B/B.B+B+B'�B �BhB\BhBJBhB�B�B{B	7B��B��B��B�B�5B��B�jB�-B��B�oB�Bl�B^5BXBL�B2-B$�B�B\B��B�ZB��BŢB�!B��B�%Bt�BZB=qB6FB.B�B
�B
��B
�jB
�-B
��B
��B
�%B
z�B
n�B
]/B
C�B
2-B
)�B
!�B
�B
�B
JB
B	�B	��B	�B	��B	��B	��B	�VB	{�B	r�B	k�B	e`B	aHB	[#B	W
B	R�B	P�B	K�B	H�B	F�B	C�B	A�B	@�B	>wB	<jB	9XB	5?B	49B	0!B	,B	'�B	�B	�B	�B	�B	oB	DB	B��B��B�B�sB�`B�ZB�NB�BB�;B�/B�B��B��B��BɺBǮBŢB��B�jB�RB�LB�FB�3B�!B�B��B��B��B��B��B��B��B�oB�VB�=B�%B�B� B~�B~�B~�B� B� B}�By�Bw�Bt�Bq�Bo�Bm�Bk�BiyBgmBgmBhsBjBk�Bl�Bk�Bk�Bk�Bk�Bl�Bk�Bk�Bl�Bl�Bl�Bk�Bm�Bn�Bn�Bo�Bp�Bq�Bq�Bq�Bs�Bu�Bx�By�Bz�B{�B{�B{�B|�B}�B~�B~�B~�B|�B}�B~�B~�B�B�B�B�B�%B�=B�JB�PB�VB�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�!B�FB�^B�^B�^B��BŢBǮB��B��B��B��B�#B�NB�ZB�fB�mB�yB�B�B�B��B��B��B	  B	B	+B	JB	bB	{B	�B	�B	$�B	&�B	)�B	-B	/B	1'B	33B	7LB	8RB	<jB	A�B	C�B	C�B	C�B	H�B	H�B	I�B	O�B	VB	XB	YB	ZB	ZB	[#B	ZB	ZB	YB	[#B	^5B	^5B	_;B	`BB	bNB	dZB	hsB	hsB	iyB	iyB	iyB	k�B	m�B	m�B	l�B	n�B	q�B	u�B	x�B	x�B	|�B	�B	�B	�B	�B	�+B	�DB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�^B	�jB	�qB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�TB	�ZB	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
\B
�B
�B
!�B
+B
2-B
:^B
?}B
G�B
L�B
S�B
YB
_;B
dZB
gmB
l�B
n�B
r�B
u�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bl�Bj�Bj�Bm�Bq�Bx�B�B�1B�DB�OB�WB�ZB�oB�rB��B��B��B��B��B��B��B��B��B��B�
B�B�)B�4B�aB�yB��B��B��B�B�YB�(B��B��BBq�Bj�BS�BI�BD�BA�B?�B<pB5BB0'B/B.B+B+B'�B �BmB^BhBRBjB�B�B�B	=B��B��B��B�B�9B��B�mB�/B��B�qB�#Bl�B^9BXBL�B20B$�B�BYB��B�\B��BŤB�"B��B�%Bt�BZ!B=vB6IB.B�B
�B
��B
�qB
�4B
��B
��B
�-B
z�B
n�B
]:B
C�B
27B
*B
!�B
�B
�B
PB
B	�B	��B	�)B	��B	��B	��B	�cB	{�B	r�B	k�B	eoB	aYB	[0B	WB	SB	P�B	K�B	H�B	F�B	C�B	A�B	@�B	>�B	<yB	9hB	5PB	4JB	01B	,B	'�B	�B	�B	�B	�B	�B	WB	4B��B��B�B�B�sB�nB�dB�WB�PB�BB�,B��B��B��B��B��BŷB��B��B�fB�bB�[B�GB�7B�%B��B��B��B��B��B��B��B��B�mB�UB�?B�+B�BBBB�B�B~By�Bw�Bt�Bq�Bo�Bm�Bk�Bi�Bg�Bg�Bh�Bj�Bk�Bl�Bk�Bk�Bk�Bk�Bl�Bk�Bk�Bl�Bl�Bl�Bk�Bm�Bn�Bn�Bo�Bp�Bq�Bq�Bq�Bs�Bu�Bx�By�Bz�B|B| B{�B}B~
BBBB}	B~
BBB� B�B�%B�4B�=B�UB�aB�gB�jB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�"B�6B�4B�5B�[B�tB�tB�uB��BŶB��B��B��B�B�B�6B�aB�nB�xB�~B�B�B��B��B��B��B��B	 B	"B	;B	ZB	tB	�B	�B	�B	$�B	&�B	*B	-B	/+B	16B	3BB	7[B	8`B	<yB	A�B	C�B	C�B	C�B	H�B	H�B	I�B	O�B	VB	XB	Y&B	Z,B	Z*B	[3B	Z,B	Z,B	Y'B	[0B	^AB	^CB	_JB	`RB	b\B	dgB	h�B	h�B	i�B	i�B	i�B	k�B	m�B	m�B	l�B	n�B	q�B	u�B	x�B	x�B	|�B	�B	�B	�B	�$B	�4B	�OB	�^B	�dB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�.B	�8B	�EB	�IB	�QB	�YB	�iB	�uB	�}B	��B	��B	��B	��B	B	áB	ĨB	ƲB	ǹB	ȾB	ȽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�,B	�2B	�;B	�7B	�?B	�EB	�EB	�SB	�WB	�^B	�^B	�cB	�oB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B
 B
B
B
B
#B
#B
(B
(B
&B
1B
/B
6B
8B
9B
	?B
	?B
	@B

FB

GB

CB
KB
LB
MB
RB
SB
dB
�B
�B
!�B
+B
25B
:fB
?�B
G�B
L�B
T B
YB
_BB
daB
gtB
l�B
n�B
r�B
u�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230519  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                