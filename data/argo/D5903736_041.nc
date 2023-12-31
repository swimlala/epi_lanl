CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:27Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230527  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               )A   AO  4051_7090_041                   2C  D   APEX                            5368                            041511                          846 @֚�}X`1   @֚��P@56ȴ9X�d����l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    )A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�fD�S3D���D�ٚD�	�D�@ D���D��fD�3D�0 D�c3Dǳ3D���D�\�DچfD�� D�fD�FfD�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z�H@���@��
A�A9�AY�Ay�A���A���A���A���A���A���A���A���Bz�Bz�Bz�B�GB&z�B.z�B6z�B>z�BFz�BNz�BVz�B^z�Bfz�Bnz�Bvz�B~z�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D g�D �Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�D	g�D	�D
g�D
�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�DnD�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�Dg�D�D g�D �D!g�D!�D"g�D"�D#g�D#�D$g�D$�D%g�D%�D&g�D&�D'g�D'�D(g�D(�D)g�D)�D*g�D*�D+g�D+�D,g�D,�D-g�D-�D.g�D.�D/g�D/�D0g�D0�D1g�D1�D2g�D2�D3g�D3�D4g�D4�D5g�D5�D6g�D6�D7g�D7�D8g�D8�D9g�D9�D:g�D:�D;g�D;�D<g�D<�D=g�D=�D>g�D>�D?g�D?�D@g�D@�DAg�DA�DBg�DB�DCg�DC�DDg�DD�DEg�DE�DFg�DF�DGg�DG�DHg�DH�DIg�DI�DJg�DJ�DKg�DK�DLg�DL�DMg�DM�DNg�DN�DOg�DO�DPaHDP�DQg�DQ�DRg�DR�DSg�DS�DTg�DT�DUg�DU�DVg�DV�DWg�DW�DXg�DX�DYg�DY�DZg�DZ�D[g�D[�D\g�D\�D]g�D]�D^g�D^�D_g�D_�D`g�D`�Dag�Da�Dbg�Db�Dcg�Dc�Ddg�Dd�Deg�De�Dfg�Df�Dgg�Dg�Dhg�Dh�Dig�Di�Djg�Dj�Dkg�Dk�Dlg�Dl�Dmg�Dm�Dng�Dn�Dog�Do�Dpg�Dp�Dqg�Dq�Drg�Dr�Dsg�Ds�Dtg�Dt�{Dyt{D��=D�G
D���D��qD��qD�3�D�}qD��=D��
D�#�D�W
Dǧ
D��D�P�D�z=D���D��=D�:=D�p�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{AǼjA��A�|�A�9XA���A�ffA���A��/A�ƨA°!A�AhA7A�z�A�^5A�G�A�/A�JA���A��#A��^A���A��!A���A�O�A���A�hsA�&�A��A��A��A�VA��`A��/A�ƨA���A�O�A�(�A���A��A���A��7A�\)A�E�A�JA���A��^A���A�r�A�C�A� �A��A���A��wA��uA�XA�&�A��A��-A�l�A�=qA�%A�r�A�9XA�  A���A��A�C�A���A�jA�9XA���A�oA��A���A�v�A�Q�A��HA� �A���A�1'A��A��hA��FA��A��A���A�VA���A�~�A�$�A�|�A��jA�dZA��uA�n�A�oA�+A�=qA��+A�XA���A�(�A���A�^5A��A��A�VA�{A��mA�oA�hsA��uA��HA�r�A��A��A��A��A�O�A�JA�;dA�JA�
=A��A��A���A�hsA��A���AG�A}?}Az�Av�Au�As�wAr�Aq�mAq�Ap�9Ao��An��AlVAj��AiAf��Ae�hAd�yAdffAa�mA_;dA]��A]XA\z�AZ�uAX��AV�+AT��ASG�AQ��AQ�FAQt�AP�!APE�APJAOt�AN5?AMXALffAH��AF�yAFA�AE�AD�\AB�/AA`BA@=qA?�#A>ȴA<  A9C�A8ffA7�A6(�A5%A3�mA2=qA1�wA1�A1|�A0z�A/|�A/\)A/
=A.��A.{A-oA*�jA)�A)S�A';dA&�/A%��A%?}A#�A!33A�PA��A�A�jA~�A�-A��A��A~�A�mAhsA�AVA�
A�`A�A+A(�A��AG�A$�A��AK�A
�9A	ƨA^5A|�A�A �Ar�A��A Z@�K�@���@�M�@�J@�?}@�r�@�\)@�O�@�l�@��H@��R@�@� �@��@�{@�G�@��/@�1@�o@�p�@��@��@��`@�I�@���@�K�@�$�@��`@㕁@�~�@���@��@�x�@�G�@�j@�\)@�{@�bN@��m@��
@ۮ@ڧ�@�7L@ؓu@ׅ@�^5@Ձ@ԣ�@� �@Ӆ@���@�n�@�$�@�x�@��m@�C�@Ώ\@��T@�?}@��`@�b@�@� �@���@��@���@�@�1'@��F@�C�@��@���@���@���@�V@�x�@�&�@� �@���@���@�G�@�1'@�l�@�~�@�Ĝ@�r�@�j@�j@�Q�@��@��@�+@�n�@��@��@��@�&�@��
@���@��y@��R@���@��R@�=q@��@�p�@�7L@���@�9X@�b@�&�@�ff@�J@��@�Q�@�|�@��@��\@�V@��@���@�G�@���@�A�@��@�1@���@��m@��@���@��@��y@�M�@�=q@�J@��#@�~�@�v�@�V@��7@��@��@��@�Q�@�9X@�1'@�b@��w@�33@��H@��@���@�ȴ@���@��R@�-@�@�33@�
=@��!@�M�@�~�@�E�@�$�@�J@���@�7L@��j@��u@�j@�A�@��@��P@�;d@���@�v�@�E�@�J@��@���@�G�@�?}@�/@�V@��j@�bN@�1'@�bN@��F@���@���@�E�@��#@�X@�V@��@�7L@���@���@��u@�Q�@��@��m@�t�@��@��H@��@���@��R@��!@���@�v�@�{@��@���@�r�@�A�@���@�ƨ@���@��@��R@��\@�n�@�5?@��T@��7@�hs@�`B@�G�@�7L@��@�%@���@��`@��`@�Ĝ@�1'@���@���@�t�@�K�@��y@��R@��!@���@���@�M�@�=q@�{@�b@�V@{@pA�@ix�@a��@Z~�@R�!@I7L@B�!@9x�@4�@/�w@,j@$��@v�@Ĝ@O�@��@@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�{AǼjA��A�|�A�9XA���A�ffA���A��/A�ƨA°!A�AhA7A�z�A�^5A�G�A�/A�JA���A��#A��^A���A��!A���A�O�A���A�hsA�&�A��A��A��A�VA��`A��/A�ƨA���A�O�A�(�A���A��A���A��7A�\)A�E�A�JA���A��^A���A�r�A�C�A� �A��A���A��wA��uA�XA�&�A��A��-A�l�A�=qA�%A�r�A�9XA�  A���A��A�C�A���A�jA�9XA���A�oA��A���A�v�A�Q�A��HA� �A���A�1'A��A��hA��FA��A��A���A�VA���A�~�A�$�A�|�A��jA�dZA��uA�n�A�oA�+A�=qA��+A�XA���A�(�A���A�^5A��A��A�VA�{A��mA�oA�hsA��uA��HA�r�A��A��A��A��A�O�A�JA�;dA�JA�
=A��A��A���A�hsA��A���AG�A}?}Az�Av�Au�As�wAr�Aq�mAq�Ap�9Ao��An��AlVAj��AiAf��Ae�hAd�yAdffAa�mA_;dA]��A]XA\z�AZ�uAX��AV�+AT��ASG�AQ��AQ�FAQt�AP�!APE�APJAOt�AN5?AMXALffAH��AF�yAFA�AE�AD�\AB�/AA`BA@=qA?�#A>ȴA<  A9C�A8ffA7�A6(�A5%A3�mA2=qA1�wA1�A1|�A0z�A/|�A/\)A/
=A.��A.{A-oA*�jA)�A)S�A';dA&�/A%��A%?}A#�A!33A�PA��A�A�jA~�A�-A��A��A~�A�mAhsA�AVA�
A�`A�A+A(�A��AG�A$�A��AK�A
�9A	ƨA^5A|�A�A �Ar�A��A Z@�K�@���@�M�@�J@�?}@�r�@�\)@�O�@�l�@��H@��R@�@� �@��@�{@�G�@��/@�1@�o@�p�@��@��@��`@�I�@���@�K�@�$�@��`@㕁@�~�@���@��@�x�@�G�@�j@�\)@�{@�bN@��m@��
@ۮ@ڧ�@�7L@ؓu@ׅ@�^5@Ձ@ԣ�@� �@Ӆ@���@�n�@�$�@�x�@��m@�C�@Ώ\@��T@�?}@��`@�b@�@� �@���@��@���@�@�1'@��F@�C�@��@���@���@���@�V@�x�@�&�@� �@���@���@�G�@�1'@�l�@�~�@�Ĝ@�r�@�j@�j@�Q�@��@��@�+@�n�@��@��@��@�&�@��
@���@��y@��R@���@��R@�=q@��@�p�@�7L@���@�9X@�b@�&�@�ff@�J@��@�Q�@�|�@��@��\@�V@��@���@�G�@���@�A�@��@�1@���@��m@��@���@��@��y@�M�@�=q@�J@��#@�~�@�v�@�V@��7@��@��@��@�Q�@�9X@�1'@�b@��w@�33@��H@��@���@�ȴ@���@��R@�-@�@�33@�
=@��!@�M�@�~�@�E�@�$�@�J@���@�7L@��j@��u@�j@�A�@��@��P@�;d@���@�v�@�E�@�J@��@���@�G�@�?}@�/@�V@��j@�bN@�1'@�bN@��F@���@���@�E�@��#@�X@�V@��@�7L@���@���@��u@�Q�@��@��m@�t�@��@��H@��@���@��R@��!@���@�v�@�{@��@���@�r�@�A�@���@�ƨ@���@��@��R@��\@�n�@�5?@��T@��7@�hs@�`B@�G�@�7L@��@�%@���@��`@��`@�Ĝ@�1'@���@���@�t�@�K�@��y@��R@��!@���@���@�M�@�=qG�O�@�b@�V@{@pA�@ix�@a��@Z~�@R�!@I7L@B�!@9x�@4�@/�w@,j@$��@v�@Ĝ@O�@��@@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��BɺB��BɺBȴBȴBȴBǮBǮBǮBǮBǮBǮBƨBƨBŢBŢBŢBÖB��B�RB�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�=B�%B}�Bw�Bs�BjBaHBXBK�BH�BF�BA�B8RB2-B+B$�B�BB�mB��BĜB��B�uB�1Bp�B\)BR�BO�BE�BA�B;dB0!B"�B�BB�B��B�XB�hBz�BjBN�BI�BE�B8RB-B �B�BbB1BB
��B
�sB
�NB
�/B
��B
�}B
�!B
��B
�uB
�+B
~�B
t�B
k�B
\)B
N�B
>wB
)�B
�B
�B
bB
DB
+B
B	��B	��B	�yB	�BB	�B	��B	ĜB	�}B	�dB	�B	��B	��B	��B	��B	�JB	�B	{�B	t�B	m�B	iyB	gmB	ffB	cTB	aHB	_;B	\)B	W
B	R�B	L�B	=qB	6FB	2-B	.B	)�B	$�B	�B	�B	�B	uB	1B��B��B��B�B�B�B�`B�TB�NB�HB�5B�#B�B�B�B��B��BB�LB�RB�B��B��B��B��B�7B�Bz�By�By�By�By�Bv�Bn�Bk�BjBiyBhsBffBe`BdZBbNB`BB`BBbNBhsBhsBiyBiyBgmBe`Be`BffBffBcTBaHB^5B]/B^5B^5B]/B]/B\)BZBYBXBYBYBXBW
BW
BVBW
BW
BW
BW
BW
BVBYBYB[#B[#B\)B]/B^5BaHBcTBe`BffBffBffBffBe`BgmBhsBiyBjBiyBiyBiyBk�Bk�Bl�Bn�Bn�Bo�Bp�Bp�Bq�Br�Br�Bs�Bu�Bv�Bv�Bw�Bx�By�Bz�B~�B�B�7B�JB�VB�\B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�wBÖBǮB��B��B��B��B��B��B��B��B�B�/B�5B�HB�TB�fB�mB�sB�sB�yB�B�B�B�B�B�B�B��B��B	B	+B	+B	B	B	B	%B	+B	1B		7B	
=B	JB	bB	�B	�B	�B	�B	"�B	#�B	$�B	'�B	)�B	,B	-B	0!B	:^B	>wB	C�B	C�B	C�B	D�B	G�B	I�B	M�B	O�B	Q�B	S�B	VB	YB	\)B	_;B	aHB	e`B	gmB	iyB	p�B	|�B	� B	�B	�B	�7B	�JB	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�-B	�-B	�-B	�3B	�3B	�3B	�LB	�qB	�}B	��B	B	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�5B	�;B	�BB	�BB	�HB	�TB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B

=B
uB
�B
$�B
+B
0!B
6FB
<jB
A�B
L�B
N�B
VB
ZB
_;B
e`B
gmB
jB
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�=B�,B�*B�B�B�B�B�B�B�'B�"B�"B�&B�(B�*B�'B�'B�'B�%B�%B�$B�&B�B�B�B�B�B�B�B�B�B�B��B��B�B� B��B��B��B��B��B��B��B��B�dB�OB~Bw�Bs�Bj�BanBX5BK�BH�BF�BA�B8yB2SB+$B%B�BBB�B�B��B�B��B�VBp�B\KBSBP BE�BA�B;�B0EB"�B�B)B�B�B�|B��B{Bj�BN�BI�BE�B8yB-5B �B�B�BXB0B
��B
�B
�vB
�ZB
�B
��B
�LB
��B
��B
�TB
&B
t�B
k�B
\QB
OB
>�B
*'B
�B
�B
�B
rB
[B
BB	�B	��B	�B	�sB	�3B	��B	��B	��B	��B	�LB	�	B	��B	��B	��B	�~B	�QB	|B	t�B	m�B	i�B	g�B	f�B	c�B	a}B	_pB	\_B	W@B	S)B	MB	=�B	6�B	2cB	.LB	*4B	%B	�B	�B	�B	�B	mB�6B�B�B��B��B�B�B�B�B�B�sB�^B�[B�MB�BB�/B�
B��B��B��B�MB�=B�B��B��B�{B�IB{"BzBzBzBzBwBn�Bk�Bj�Bi�Bh�Bf�Be�Bd�Bb�B`�B`�Bb�Bh�Bh�Bi�Bi�Bg�Be�Be�Bf�Bf�Bc�Ba�B^zB]tB^{B^xB]tB]rB\lBZaBY[BXVBY\BY]BXVBWPBWQBVMBWPBWPBWQBWOBWPBVGBY\BY\B[hB[iB\mB]uB^zBa�Bc�Be�Bf�Bf�Bf�Bf�Be�Bg�Bh�Bi�Bj�Bi�Bi�Bi�Bk�Bk�Bl�Bn�Bn�Bo�Bp�Bp�Bq�Br�Br�Bs�Bv
BwBwBxByBz!B{%B<B�]B�|B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�OB�eB��B��B��B��B�B�B�B� B�#B�&B�3B�AB�^B�qB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B�B�:B	_B	jB	hB	^B	YB	^B	bB	mB	rB		vB	
}B	�B	�B	�B	�B	�B	�B	#B	$B	%B	(.B	*<B	,HB	-MB	0aB	:�B	>�B	C�B	C�B	C�B	D�B	G�B	I�B	NB	PB	R(B	T4B	VAB	YTB	\fB	_{B	a�B	e�B	g�B	i�B	p�B	}(B	�;B	�IB	�NB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�1B	�AB	�PB	�NB	�SB	�\B	�UB	�jB	�eB	�gB	�kB	�nB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�#B	�,B	�,B	�0B	�7B	�BB	�VB	�hB	�oB	�sB	�{B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B

tB
�B
�B
%B
+;B
0WB
6�B
<�B
A�B
MB
OB
V8B
ZTB
_rB
e�B
g�B
j�B
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.38 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230527    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230527  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230527  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                