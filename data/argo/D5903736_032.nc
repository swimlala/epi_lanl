CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:23Z AOML 3.0 creation; 2016-05-31T19:14:29Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230523  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  4051_7090_032                   2C  D   APEX                            5368                            041511                          846 @փi���1   @փj4%��@3�C���d��$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   A   A   @9��@y��@�  @���AffA@  A`  A���A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�3D�FfD�� D��3D� D�Y�D�� D��fD� D�I�D�� D��fD�fD�S3D�s3D�ɚD��fD�@ D�ffD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @4z�@tz�@�p�@�=qA�A>�RA^�RA�(�A�\)A��\A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=B�
=B��
B��
Bã�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C"C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�CnCo�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,�GD,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DPGDPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dyz�D��D�C�D�}qD���D�qD�WD��qD���D�qD�GD��qD���D��D�P�D�p�D��D���D�=qD�c�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A� �A� �A�"�A�"�A�"�A�&�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�"�A��A�A�
=A��A�  A��A��;A��yA���AȅA�-Aǉ7A�?}A�(�A���A��A��A��mAŴ9A�1AÝ�A�%A�O�A��
A���A���A���A��A���A��#A��A���A���A�jA���A�Q�A��DA�&�A�A���A��+A�z�A�p�A�=qA���A���A�5?A�|�A��
A�A�A��7A��A��;A�7LA�A��A�ĜA�JA��wA�VA��
A��A�5?A���A��hA�1A�+A��RA���A�\)A��-A���A�oA�S�A���A�JA��HA��A�n�A�oA�{A���A��A���A�x�A�C�A���A��\A�;dA���A���A���A���A�ZA���A~~�Ax��As�-Ap��An��Am`BAk`BAj �Ag|�Ad�A_C�A\�A[ƨA[%AX�`AW`BAVȴAU
=AS��ASl�AR�ARZAR{AQ�;AQhsAO&�AK�#AK�AK��AI33AHbAGXAFE�AD��ACXAB-A@��A?�TA?hsA>v�A<bNA:�DA8�A6-A57LA4��A4�A2��A2  A0��A/hsA-�FA*�RA(�jA'��A'x�A'/A&r�A&{A%�#A%oA#��A#l�A"�A!dZA �A�AO�A��A �A �A��A�HA-A�A|�A��AVA�RAJAI�A|�A1'A&�A
=qAZAr�AƨA��A�TA�AoA�DA��A7LA ��A -@��R@�?}@�bN@�;d@���@�r�@�S�@�ȴ@��@���@�ff@�(�@�@띲@�!@��@��@�@�J@�Z@�  @���@�\)@�\@���@�ƨ@���@ۍP@�5?@٩�@��@�Ĝ@؃@׾w@��@Ӯ@�`B@�I�@�@���@�o@�
=@�{@��@���@�J@�@�z�@�ƨ@�dZ@Ƈ+@�{@��@š�@�O�@���@ă@�9X@�(�@�K�@�V@��T@��@�33@���@�^5@�%@�O�@�^5@�ȴ@���@���@�@��@��@�{@���@���@��w@�l�@��R@���@���@���@��7@�1'@��@��y@��@�~�@��-@��@�bN@�b@���@�l�@�C�@���@���@���@��+@�V@�@�I�@�A�@�A�@��@�ƨ@�l�@�  @�z�@�bN@�A�@�9X@���@��@��@���@�K�@��H@�M�@��h@��/@�Q�@�(�@���@���@�33@�n�@��@�O�@���@��@�A�@�\)@���@�ȴ@��@�@�/@�/@���@��u@��@�z�@���@� �@��@���@���@�C�@���@��@�J@�J@�@���@�`B@�G�@��@���@��@���@��u@�bN@�I�@�1@���@��P@�C�@��R@���@�$�@���@��-@�&�@�Ĝ@��@��u@�r�@�Z@�I�@�b@��P@��@��R@���@��+@�~�@�n�@�V@�-@�-@���@��h@�hs@�7L@�V@��@��/@���@��@�bN@�1@��
@��F@��@���@�|�@�C�@��H@��!@��+@�M�@�$�@��T@��7@��@�x�@�x�@�G�@���@���@��9@�bN@��
@���@��@�dZ@�S�@��@�E�@�5?@��@���@��h@�X@�/@�%@���@���@���@�z�@�Z@�Q�@�(�@��m@�ƨ@���@��P@�"�@��R@�=q@�$�@��@��T@��^@���@��h@��@�X@�&�@���@���@��@��D@��@�z�@�j@�(�@���@��
@�dZ@�;d@��@��y@��!@�/@z�@r�@jM�@a��@ZM�@SC�@Kƨ@B��@:^5@4�D@/
=@)hs@$(�@��@�@{@�@�-@o@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A� �A� �A�"�A�"�A�"�A�&�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�"�A��A�A�
=A��A�  A��A��;A��yA���AȅA�-Aǉ7A�?}A�(�A���A��A��A��mAŴ9A�1AÝ�A�%A�O�A��
A���A���A���A��A���A��#A��A���A���A�jA���A�Q�A��DA�&�A�A���A��+A�z�A�p�A�=qA���A���A�5?A�|�A��
A�A�A��7A��A��;A�7LA�A��A�ĜA�JA��wA�VA��
A��A�5?A���A��hA�1A�+A��RA���A�\)A��-A���A�oA�S�A���A�JA��HA��A�n�A�oA�{A���A��A���A�x�A�C�A���A��\A�;dA���A���A���A���A�ZA���A~~�Ax��As�-Ap��An��Am`BAk`BAj �Ag|�Ad�A_C�A\�A[ƨA[%AX�`AW`BAVȴAU
=AS��ASl�AR�ARZAR{AQ�;AQhsAO&�AK�#AK�AK��AI33AHbAGXAFE�AD��ACXAB-A@��A?�TA?hsA>v�A<bNA:�DA8�A6-A57LA4��A4�A2��A2  A0��A/hsA-�FA*�RA(�jA'��A'x�A'/A&r�A&{A%�#A%oA#��A#l�A"�A!dZA �A�AO�A��A �A �A��A�HA-A�A|�A��AVA�RAJAI�A|�A1'A&�A
=qAZAr�AƨA��A�TA�AoA�DA��A7LA ��A -@��R@�?}@�bN@�;d@���@�r�@�S�@�ȴ@��@���@�ff@�(�@�@띲@�!@��@��@�@�J@�Z@�  @���@�\)@�\@���@�ƨ@���@ۍP@�5?@٩�@��@�Ĝ@؃@׾w@��@Ӯ@�`B@�I�@�@���@�o@�
=@�{@��@���@�J@�@�z�@�ƨ@�dZ@Ƈ+@�{@��@š�@�O�@���@ă@�9X@�(�@�K�@�V@��T@��@�33@���@�^5@�%@�O�@�^5@�ȴ@���@���@�@��@��@�{@���@���@��w@�l�@��R@���@���@���@��7@�1'@��@��y@��@�~�@��-@��@�bN@�b@���@�l�@�C�@���@���@���@��+@�V@�@�I�@�A�@�A�@��@�ƨ@�l�@�  @�z�@�bN@�A�@�9X@���@��@��@���@�K�@��H@�M�@��h@��/@�Q�@�(�@���@���@�33@�n�@��@�O�@���@��@�A�@�\)@���@�ȴ@��@�@�/@�/@���@��u@��@�z�@���@� �@��@���@���@�C�@���@��@�J@�J@�@���@�`B@�G�@��@���@��@���@��u@�bN@�I�@�1@���@��P@�C�@��R@���@�$�@���@��-@�&�@�Ĝ@��@��u@�r�@�Z@�I�@�b@��P@��@��R@���@��+@�~�@�n�@�V@�-@�-@���@��h@�hs@�7L@�V@��@��/@���@��@�bN@�1@��
@��F@��@���@�|�@�C�@��H@��!@��+@�M�@�$�@��T@��7@��@�x�@�x�@�G�@���@���@��9@�bN@��
@���@��@�dZ@�S�@��@�E�@�5?@��@���@��h@�X@�/@�%@���@���@���@�z�@�Z@�Q�@�(�@��m@�ƨ@���@��P@�"�@��R@�=q@�$�@��@��T@��^@���@��h@��@�X@�&�@���@���@��@��D@��@�z�@�j@�(�@���@��
@�dZ@�;d@��@��y@��!@�/@z�@r�@jM�@a��@ZM�@SC�@Kƨ@B��@:^5@4�D@/
=@)hs@$(�@��@�@{@�@�-@o@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B-B,B,B-B,B-B.B/B49B5?B5?B5?B5?B5?B49B0!B/B8RB;dB=qBG�BT�B\)BI�B49BC�B9XB9XB9XB8RB7LB/B"�B�B�B{BuBuBoBhB\BJB%BB��B��B��B�B�B�B��B�B�yB�BB�5B�)B��B�jB�'B��B�uB�bB�7Bx�Bu�Bu�Bp�BZBJ�B�B�B��BB�B��B�oBz�BhsBM�B�BbBJB1BB
��B
��B
�yB
��B
��B
�B
l�B
R�B
-B
1B	�`B	��B	ƨB	��B	�?B	��B	��B	�B	ffB	W
B	S�B	N�B	E�B	?}B	;dB	6FB	7LB	33B	/B	)�B	&�B	#�B	�B	�B	�B	 �B	"�B	�B	oB	VB	+B	  B��B�B�B�`B�NB�5B��B��B��BĜB��B��B��B��B�}B�wB�}B�wB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�%B�B�B� B|�By�Bx�Bv�Bt�Bp�Bn�Bk�BjBiyBgmBhsBgmBe`Be`Be`Be`Be`Be`Be`Be`BcTBbNBcTBbNBbNBbNBcTBdZBdZBcTBcTBbNBaHBdZBffBe`Be`BgmBgmBk�Bk�BjBjBiyBiyBjBiyBm�Bn�Bo�Bs�Bu�Bt�Bt�Bs�Bs�Br�Br�Bw�B{�B� B�B�+B�=B�7B�DB�\B�\B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�-B�B�B�B��B�!B�dBBƨBɺBɺBȴB��B��B��B�B�BB�ZB�`B�TB�sB�B�yB�`B�NB�`B�`B�fB�sB�fB�`B�B�B�B��B��B��B	%B	+B		7B	JB	\B	{B	�B	!�B	#�B	"�B	'�B	2-B	7LB	8RB	;dB	>wB	?}B	?}B	?}B	A�B	C�B	G�B	M�B	P�B	VB	XB	YB	[#B	]/B	`BB	cTB	e`B	ffB	gmB	iyB	iyB	iyB	o�B	p�B	q�B	r�B	s�B	u�B	v�B	w�B	|�B	}�B	�B	�1B	�1B	�+B	�1B	�7B	�DB	�JB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�XB	�}B	B	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�5B	�;B	�BB	�HB	�HB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
\B
�B
�B
)�B
0!B
6FB
<jB
D�B
K�B
P�B
VB
[#B
_;B
dZB
jB
n�B
q�B
v�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B-B,B,B-B,B-B.B/&B4EB5MB5LB5GB5KB5KB4EB0+B/'B8^B;qB=}BG�BUB\5BI�B4EBC�B9aB9`B9`B8^B7WB/#B"�B�B�B�BBByBnBgBSB/BB��B��B��B�B�B��B��B�B�B�KB�>B�4B��B�uB�/B��B�B�iB�?Bx�Bu�Bu�Bp�BZ#BJ�B�B�B��BB�B��B�wBz�Bh~BM�B�BmBSB:BB
��B
��B
�B
��B
��B
�B
l�B
R�B
-B
AB	�oB	�
B	ƸB	��B	�PB	�B	��B	�!B	f|B	WB	TB	N�B	E�B	?�B	;{B	6^B	7cB	3LB	/2B	*B	'B	#�B	�B	�B	�B	 �B	"�B	�B	�B	nB	DB	 B��B��B�B�yB�jB�RB�B��B��BĹB��B��B��B��B��B��B��B��B�OB�-B�#B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�eB�DB�7B�2B� B}By�Bx�Bv�Bt�Bp�Bn�Bk�Bj�Bi�Bg�Bh�Bg�Be�Be�Be�Be�Be�Be�Be�Be�BcuBbnBcuBbnBbqBboBcvBdyBd}BcuBcvBbpBaiBdzBf�Be�Be�Bg�Bg�Bk�Bk�Bj�Bj�Bi�Bi�Bj�Bi�Bm�Bn�Bo�Bs�Bu�Bt�Bt�Bs�Bs�Br�Br�Bw�B|B�B�*B�IB�]B�YB�dB�}B�{B��B��B��B��B��B�B�B�B�B�B�B�'B�+B�6B�UB�IB�2B�+B�%B�B�?B��B®B��B��B��B��B��B�B�B�,B�]B�sB�zB�nB�B�B�B�yB�hB�vB�zB�B�B�B�{B�B�B��B��B��B��B	=B	DB		QB	aB	uB	�B	�B	!�B	#�B	"�B	(B	2BB	7cB	8jB	;}B	>�B	?�B	?�B	?�B	A�B	C�B	G�B	M�B	P�B	VB	X&B	Y.B	[:B	]FB	`XB	chB	euB	fzB	g�B	i�B	i�B	i�B	o�B	p�B	q�B	r�B	s�B	u�B	v�B	w�B	}B	~	B	�#B	�EB	�EB	�>B	�GB	�JB	�WB	�^B	�jB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�#B	� B	�5B	�-B	�(B	�(B	�/B	�5B	�8B	�HB	�IB	�YB	�kB	��B	¢B	ıB	ĮB	ŶB	ŵB	ŶB	ƻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�%B	�1B	�6B	�;B	�GB	�GB	�MB	�GB	�LB	�SB	�\B	�\B	�fB	�sB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�	B
B
B
B
B
"B
+B
+B
8B
6B
5B
5B
>B
AB
mB
�B
�B
*
B
0/B
6TB
<xB
D�B
K�B
P�B
VB
[1B
_GB
dhB
j�B
n�B
q�B
v�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230523  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                