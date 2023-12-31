CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:25Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230525  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  4051_7090_036                   2C  D   APEX                            5368                            041511                          846 @֍�w���1   @֍�r�
@4)��l�D�d{�E���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dys3D�fD�L�D�vfD���D�	�D�@ D�y�D�� D�3D�FfD�i�D��fD�fD�S3DږfD��fD���D�,�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cn{Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D
D
�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��DtxRDyq�D��D�L)D�u�D��)D��D�?\D�x�D��\D��D�E�D�h�D���D��D�R�Dڕ�D���D��)D�,)D�|)D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�O�A�M�A�K�A�M�A�M�A�K�A�Q�A�Q�A�Q�A�K�A�M�A�E�A�"�A���A�p�A�  A���A�AöFAîAìAé�Aç�Aé�AîA�x�A�K�A�A�A�"�A��yA¶FA£�AA�VA�5?A�-A��A��+A��A���A�1'A�ƨA��DA�x�A��
A��wA�v�A�+A�M�A��FA�+A���A�^5A�A�A���A�1'A�M�A��HA�JA�t�A�O�A��A�1'A��A���A��A�^5A���A���A�`BA��A��RA�(�A�x�A�-A��TA��uA�`BA�&�A��7A���A��A�5?A��A�A�A�?}A���A�1'A��A���A���A��A�JA��hA��A��`A�Q�A�/A�VA�  A��+A�A���A�VA�x�A�=qA�5?A�z�A��-A��A�|�A�=qA�ffA���A��yA�A~��A}��A|z�A{`BAz�yAzI�Ax �Au��At�RAs��Ap�\Am�Al��Aj�yAh��Ag��Ae�FAbQ�A`A�A^=qA[p�AXĜAWVAT�\AQ�AQ��AO�^AM�hAL�HALA�AK�hAK"�AJ�RAJ��AJ��AJ��AIO�AHv�AGG�AE;dAD=qAC"�AAdZA>z�A;�-A:��A8��A6�A5;dA4�A4�uA4-A333A21A1VA0�A/G�A-�;A,Q�A+G�A)��A(�A'��A&��A#��A"�!A"M�A!�7A �A�RAXA�A �A��AM�A��A\)A �A9XA/AbA�A�AdZA��A1Ax�A
��A
A	��A	A��A �A�RA��AA|�A�A�+A�A �@�ȴ@�n�@��@��
@��@���@��
@��#@��@��@��@�K�@��@��m@�@�o@�X@��@�@�V@�@��@�7L@�j@��@�n�@�O�@�Ĝ@�  @��@�1@�M�@�hs@ج@��
@���@��T@�@�{@�E�@��@�@ԛ�@ӥ�@�V@��@�  @ˮ@���@�E�@���@�dZ@�|�@ǥ�@�C�@�J@�O�@�V@�1@�S�@���@�%@�t�@��@��j@�;d@��-@��@�Q�@�9X@��+@��T@�%@� �@�1'@��F@�l�@��H@���@��@��7@���@��T@�G�@��7@�?}@�/@�ƨ@��y@��H@�o@�C�@��@���@���@�5?@��#@��@��/@�Z@�7L@�J@��@��D@�j@���@�b@�S�@�K�@��@�z�@�?}@��@� �@��;@��@�@��@�~�@�M�@�ff@��H@�+@���@��@��#@��@���@�"�@�K�@�n�@�-@���@�p�@�/@�V@��@�&�@�V@��@�1@��F@��w@�C�@��!@�{@���@��^@��h@�G�@�%@�Ĝ@��9@��j@��9@��9@��j@�Ĝ@���@�r�@�9X@�b@��w@��F@�l�@���@���@�V@�5?@�-@�{@�@��@��-@��7@�hs@�O�@�/@�&�@���@��D@�j@�Q�@�I�@�9X@�1'@� �@��m@�K�@���@�ff@�J@���@�@��7@�/@��@�V@�%@���@�bN@���@�ƨ@��@���@���@�S�@�;d@�
=@���@�ff@�=q@�@�@�x�@�7L@��@��/@��/@���@�r�@�  @�ƨ@�33@��H@���@���@�~�@��+@�J@���@�p�@�/@��@��`@�Ĝ@��@�Q�@�b@�b@�1@���@���@�\)@�"�@��@�ȴ@�~�@��@��-@���@�x�@�G�@��@�r�@�r�@�A�@�1@��m@�ƨ@��P@�@��@��H@��y@���@�M�@��@��-@�`B@�dZ@}�T@sƨ@k�F@e�@^��@Vff@M?}@Ep�@?\)@8bN@1�@-?}@'�@ �`@1@��@��@�@�m@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�O�A�M�A�K�A�M�A�M�A�K�A�Q�A�Q�A�Q�A�K�A�M�A�E�A�"�A���A�p�A�  A���A�AöFAîAìAé�Aç�Aé�AîA�x�A�K�A�A�A�"�A��yA¶FA£�AA�VA�5?A�-A��A��+A��A���A�1'A�ƨA��DA�x�A��
A��wA�v�A�+A�M�A��FA�+A���A�^5A�A�A���A�1'A�M�A��HA�JA�t�A�O�A��A�1'A��A���A��A�^5A���A���A�`BA��A��RA�(�A�x�A�-A��TA��uA�`BA�&�A��7A���A��A�5?A��A�A�A�?}A���A�1'A��A���A���A��A�JA��hA��A��`A�Q�A�/A�VA�  A��+A�A���A�VA�x�A�=qA�5?A�z�A��-A��A�|�A�=qA�ffA���A��yA�A~��A}��A|z�A{`BAz�yAzI�Ax �Au��At�RAs��Ap�\Am�Al��Aj�yAh��Ag��Ae�FAbQ�A`A�A^=qA[p�AXĜAWVAT�\AQ�AQ��AO�^AM�hAL�HALA�AK�hAK"�AJ�RAJ��AJ��AJ��AIO�AHv�AGG�AE;dAD=qAC"�AAdZA>z�A;�-A:��A8��A6�A5;dA4�A4�uA4-A333A21A1VA0�A/G�A-�;A,Q�A+G�A)��A(�A'��A&��A#��A"�!A"M�A!�7A �A�RAXA�A �A��AM�A��A\)A �A9XA/AbA�A�AdZA��A1Ax�A
��A
A	��A	A��A �A�RA��AA|�A�A�+A�A �@�ȴ@�n�@��@��
@��@���@��
@��#@��@��@��@�K�@��@��m@�@�o@�X@��@�@�V@�@��@�7L@�j@��@�n�@�O�@�Ĝ@�  @��@�1@�M�@�hs@ج@��
@���@��T@�@�{@�E�@��@�@ԛ�@ӥ�@�V@��@�  @ˮ@���@�E�@���@�dZ@�|�@ǥ�@�C�@�J@�O�@�V@�1@�S�@���@�%@�t�@��@��j@�;d@��-@��@�Q�@�9X@��+@��T@�%@� �@�1'@��F@�l�@��H@���@��@��7@���@��T@�G�@��7@�?}@�/@�ƨ@��y@��H@�o@�C�@��@���@���@�5?@��#@��@��/@�Z@�7L@�J@��@��D@�j@���@�b@�S�@�K�@��@�z�@�?}@��@� �@��;@��@�@��@�~�@�M�@�ff@��H@�+@���@��@��#@��@���@�"�@�K�@�n�@�-@���@�p�@�/@�V@��@�&�@�V@��@�1@��F@��w@�C�@��!@�{@���@��^@��h@�G�@�%@�Ĝ@��9@��j@��9@��9@��j@�Ĝ@���@�r�@�9X@�b@��w@��F@�l�@���@���@�V@�5?@�-@�{@�@��@��-@��7@�hs@�O�@�/@�&�@���@��D@�j@�Q�@�I�@�9X@�1'@� �@��m@�K�@���@�ff@�J@���@�@��7@�/@��@�V@�%@���@�bN@���@�ƨ@��@���@���@�S�@�;d@�
=@���@�ff@�=q@�@�@�x�@�7L@��@��/@��/@���@�r�@�  @�ƨ@�33@��H@���@���@�~�@��+@�J@���@�p�@�/@��@��`@�Ĝ@��@�Q�@�b@�b@�1@���@���@�\)@�"�@��@�ȴ@�~�@��@��-@���@�x�@�G�@��@�r�@�r�@�A�@�1@��m@�ƨ@��P@�@��@��H@��y@���@�M�@��@��-@�`B@�dZ@}�T@sƨ@k�F@e�@^��@Vff@M?}@Ep�@?\)@8bN@1�@-?}@'�@ �`@1@��@��@�@�m@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�#B�#B�/B�`B��BB�B�B�B �B!�B!�B!�B"�B"�B)�B>wBE�BK�BW
B\)BdZBe`BgmBiyBiyBiyBiyBjBm�Bq�B~�B�%B�B�B{�B<jB �B�B�B%�B"�B�B�B\B1B��B��B�B�B�B�ZB��B�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�9B�B�%Bn�B[#BO�BD�B&�B  B�;BȴB�jB�?B��B�VB�Bp�B_;BL�B8RB"�BuB1B
��B
�B
�/B
��B
�wB
�FB
�B
��B
�B
q�B
cTB
L�B
;dB
33B
/B
,B
,B
(�B
�B
VB
%B	��B	�yB	�B	��B	ĜB	�?B	�B	��B	�7B	w�B	bNB	YB	K�B	D�B	1'B	)�B	B�B	E�B	?}B	<jB	6FB	1'B	.B	-B	.B	0!B	.B	$�B	!�B	�B	{B	bB	DB	B��B�B�yB�#B��B��B��B��B��B��BŢB�}B�}B�jB�LB�'B�B��B��B��B��B�\B�PB�JB�=B�+B�%B�B�B�B{�Bx�Bw�Bt�Bs�Bp�Bm�BiyBffBcTBcTBbNBaHBaHB`BBaHB`BB_;B\)BYBXBT�BT�BT�BS�BQ�BQ�BP�BO�BN�BL�BK�BK�BK�BJ�BI�BH�BH�BG�BI�BK�BK�BJ�BJ�BJ�BI�BI�BH�BH�BJ�BK�BJ�BJ�BK�BK�BK�BJ�BM�BM�BO�BP�BP�BR�BYBbNBffBk�Br�Bv�B}�B�B�B�Bz�Bv�Bu�Br�Bp�Bt�B{�B�B�%B�1B�JB�\B�VB�PB�\B��B��B��B��B��B��B��B�B�B�9B�^BĜBƨBǮB��B��B��B�ZB�sB�yB�B�B�B�B��B	B	B��B��B	B	%B	
=B	PB	bB	�B	�B	�B	�B	�B	�B	�B	JB	PB	\B	bB	oB	oB	{B	�B	�B	 �B	%�B	#�B	"�B	#�B	&�B	,B	1'B	49B	:^B	<jB	@�B	C�B	B�B	F�B	H�B	K�B	VB	ZB	[#B	_;B	gmB	n�B	o�B	p�B	p�B	t�B	u�B	v�B	y�B	|�B	~�B	�B	�B	�B	�%B	�%B	�7B	�DB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�9B	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�qB	�}B	��B	��B	ÖB	ÖB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�5B	�;B	�NB	�TB	�`B	�fB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
\B
�B
 �B
%�B
+B
1'B
9XB
A�B
F�B
L�B
R�B
W
B
^5B
e`B
iyB
m�B
p�B
s�B
w�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B� B� B�$B�&B�6B�dB��BB�B�B�B �B!�B!�B!�B"�B"�B)�B>~BE�BK�BWB\/BdaBefBguBi~BiBi}Bi~Bj�Bm�Bq�BB�+B�"B�B{�B<qB �B�B�B%�B"�B�B�B_B6B��B��B�B�B�B�ZB��B�4B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�.B�@B�B�&Bn�B[%BO�BD�B&�B  B�<BȶB�mB�AB��B�WB�Bp�B_>BL�B8UB"�ByB5B
��B
�B
�3B
��B
�~B
�JB
�B
��B
�'B
q�B
cZB
L�B
;mB
3>B
/"B
,B
,B
(�B
�B
`B
0B	��B	�B	�B	��B	ĩB	�IB	�'B	��B	�EB	w�B	b\B	Y(B	K�B	D�B	18B	*B	B�B	E�B	?�B	<yB	6VB	19B	.$B	-B	.&B	03B	.&B	$�B	!�B	�B	�B	uB	UB	%B��B��B�B�6B�	B��B��B�B��B��BŹB��B��B��B�`B�;B�B��B��B��B��B�vB�gB�bB�TB�CB�>B�1B�*B�B{�Bx�Bw�Bt�Bs�Bp�Bm�Bi�Bf�BcmBcnBbfBaaBaaB`]BaaB`\B_VB\BBY3BX,BUBUBUBTBRBRBQ BO�BN�BL�BK�BK�BK�BJ�BI�BH�BH�BG�BI�BK�BK�BJ�BJ�BJ�BI�BI�BH�BH�BJ�BK�BJ�BJ�BK�BK�BK�BJ�BM�BM�BO�BP�BQ BSBY1BbhBf~Bk�Br�Bv�B~
B�B� B�Bz�Bv�Bu�Br�Bp�Bt�B{�B�B�:B�IB�`B�tB�kB�fB�qB��B��B��B��B��B��B��B�B�$B�OB�uBĲBƺB��B��B��B�B�nB�B�B�B�B��B��B��B	$B	+B�B�B	%B	6B	
MB	bB	sB	�B	�B	�B	�B	�B	�B	�B	[B	bB	mB	sB	�B	�B	�B	�B	�B	 �B	%�B	#�B	"�B	#�B	&�B	,B	17B	4JB	:pB	<yB	@�B	C�B	B�B	F�B	H�B	K�B	VB	Z+B	[4B	_HB	gyB	n�B	o�B	p�B	p�B	t�B	u�B	v�B	y�B	|�B	B	�B	�B	�*B	�2B	�/B	�DB	�OB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�-B	�2B	�2B	�6B	�@B	�?B	�CB	�[B	�\B	�bB	�cB	�dB	�lB	�lB	�pB	�zB	��B	��B	��B	àB	ßB	ŮB	ǹB	ǸB	ȿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�(B	�4B	�?B	�FB	�XB	�\B	�kB	�sB	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
B
B
B
B
B
1B
eB
�B
 �B
%�B
+B
11B
9aB
A�B
F�B
L�B
R�B
WB
^=B
ehB
i~B
m�B
p�B
s�B
w�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230525    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230525  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230525  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                