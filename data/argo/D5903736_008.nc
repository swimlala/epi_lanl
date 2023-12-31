CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:10Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
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
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230510  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_008                   2C  D   APEX                            5368                            041511                          846 @�E�U��	1   @�E��Ѐ@3e�S����d!`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy��D�3D�S3D�� D��fD�3D�9�D��3D�� D��fD�6fD�� DǼ�D�	�D�0 Dډ�D�� D��D�9�D�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/G�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C:C<C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�GD��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds�Dy�{D��D�P�D�}qD���D��D�7D���D��qD���D�3�D��qDǺ>D�D�-qDڇD�qD�
>D�7D�D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�ffA��A���Aʲ-AʓuA�z�A�t�A�l�A�^5A�VA�33A�oA�v�A��mA�`BA�E�A�(�A�K�A�oA��HAȰ!A�ƨA���A��A��yA��`A��A�Aȴ9A�1'A�bA�  AǼjA���A�?}A�%A��A��A�A�"�A���A�^5A�33A�"�A��#A�z�A�/A���A�=qA���A���A�x�A�%A���A�JA���A�p�A�=qA� �A��hA��A���A�dZA��A�l�A�bNA�XA�O�A��A�bA�ƨA���A���A�hsA�(�A��`A��TA��A��9A�r�A���A�ĜA�+A�I�A�S�A�ȴA��9A��TA��A��-A�7LA�{A�~�A�r�A���A�hsA��A�l�A���A�`BA�|�A��A�  A��hA�r�A�"�A�S�A��A��A���A��DA��+A��A��wA�%A�`BA�A�{A�-A~E�Az�AzE�AvbNAr�ApĜAn�uAlȴAk"�Ai�-Ag��Ad�Ac�AcS�AbjAaS�A^�9A]S�A\��A[��AY|�AV��AT�DAR��AQ+AN9XAL�AJ��AH��AH9XAG��AE�mADQ�AB{A@�\A?&�A=C�A<�\A<��A;K�A9`BA8�DA7A6��A5%A3��A29XA0�`A/G�A.�A-�A,�+A+oA)�PA(^5A&=qA%dZA%"�A$^5A#G�A"M�A!O�A 1A�\A��A�PA�uA�hAbNA�!A��AjAt�A�A\)A?}A�A��AXA��A
�yA
bA	x�A��A��A�hA�A�A+AM�A��A9XA��AV@��
@�$�@�{@�-@�G�@��@�G�@�1'@�"�@�5?@�?}@���@��@��@�E�@�@�O�@���@���@�@�@�$�@��T@�x�@�V@�9@�D@�bN@�9X@���@�
=@��@�/@�bN@��
@��;@�ƨ@�-@��@�j@�ƨ@���@���@��@�l�@�V@݁@��/@�
=@�G�@֟�@���@�?}@ԓu@�Z@�Q�@���@�"�@�~�@���@��@�+@�J@�?}@��
@�=q@�z�@ǅ@Ƈ+@�$�@�/@�j@Å@¸R@��@�7L@�(�@��!@�-@�`B@�(�@�S�@���@��@��@�l�@�
=@�^5@��@��u@��m@�|�@�C�@��@��R@�~�@��T@���@�?}@���@��@�A�@��;@�t�@�C�@�"�@��@��+@�V@�5?@��@�x�@�`B@�X@�?}@��@���@�Q�@��@��P@�;d@��!@�{@�@�p�@���@��9@���@��u@�9X@���@���@��@�x�@�p�@�V@��`@��/@���@��j@��u@�Z@�(�@��@���@�dZ@�K�@�l�@��@�"�@���@�^5@�@���@��h@�x�@�hs@��`@���@��@�z�@�r�@�z�@�z�@��@�Z@��@��w@���@��P@�K�@�"�@�@���@��!@�$�@���@���@�p�@�G�@��@�V@�%@��@���@��@���@��D@� �@���@��P@�l�@�K�@�
=@��R@���@�-@��T@��h@�X@�&�@�%@���@���@�(�@�ƨ@�t�@�;d@�
=@���@�E�@��@��^@�G�@���@��j@��@��j@�j@�b@�t�@�dZ@�;d@�o@���@��\@�^5@�E�@��@��#@��-@�hs@�%@��j@��j@���@�A�@� �@��m@���@�t�@�S�@�"�@���@��H@��@���@��+@��@��^@���@��@�X@�V@��/@���@��D@�j@��@��@�|�@�S�@�C�@��@��y@���@�ff@�M�@��#@��7@�7L@��@��@��R@|�@u��@l9X@c"�@\(�@UV@N�R@H��@B��@;dZ@4�@.��@'\)@"-@9X@��@S�@A�@@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�"�A�ffA��A���Aʲ-AʓuA�z�A�t�A�l�A�^5A�VA�33A�oA�v�A��mA�`BA�E�A�(�A�K�A�oA��HAȰ!A�ƨA���A��A��yA��`A��A�Aȴ9A�1'A�bA�  AǼjA���A�?}A�%A��A��A�A�"�A���A�^5A�33A�"�A��#A�z�A�/A���A�=qA���A���A�x�A�%A���A�JA���A�p�A�=qA� �A��hA��A���A�dZA��A�l�A�bNA�XA�O�A��A�bA�ƨA���A���A�hsA�(�A��`A��TA��A��9A�r�A���A�ĜA�+A�I�A�S�A�ȴA��9A��TA��A��-A�7LA�{A�~�A�r�A���A�hsA��A�l�A���A�`BA�|�A��A�  A��hA�r�A�"�A�S�A��A��A���A��DA��+A��A��wA�%A�`BA�A�{A�-A~E�Az�AzE�AvbNAr�ApĜAn�uAlȴAk"�Ai�-Ag��Ad�Ac�AcS�AbjAaS�A^�9A]S�A\��A[��AY|�AV��AT�DAR��AQ+AN9XAL�AJ��AH��AH9XAG��AE�mADQ�AB{A@�\A?&�A=C�A<�\A<��A;K�A9`BA8�DA7A6��A5%A3��A29XA0�`A/G�A.�A-�A,�+A+oA)�PA(^5A&=qA%dZA%"�A$^5A#G�A"M�A!O�A 1A�\A��A�PA�uA�hAbNA�!A��AjAt�A�A\)A?}A�A��AXA��A
�yA
bA	x�A��A��A�hA�A�A+AM�A��A9XA��AV@��
@�$�@�{@�-@�G�@��@�G�@�1'@�"�@�5?@�?}@���@��@��@�E�@�@�O�@���@���@�@�@�$�@��T@�x�@�V@�9@�D@�bN@�9X@���@�
=@��@�/@�bN@��
@��;@�ƨ@�-@��@�j@�ƨ@���@���@��@�l�@�V@݁@��/@�
=@�G�@֟�@���@�?}@ԓu@�Z@�Q�@���@�"�@�~�@���@��@�+@�J@�?}@��
@�=q@�z�@ǅ@Ƈ+@�$�@�/@�j@Å@¸R@��@�7L@�(�@��!@�-@�`B@�(�@�S�@���@��@��@�l�@�
=@�^5@��@��u@��m@�|�@�C�@��@��R@�~�@��T@���@�?}@���@��@�A�@��;@�t�@�C�@�"�@��@��+@�V@�5?@��@�x�@�`B@�X@�?}@��@���@�Q�@��@��P@�;d@��!@�{@�@�p�@���@��9@���@��u@�9X@���@���@��@�x�@�p�@�V@��`@��/@���@��j@��u@�Z@�(�@��@���@�dZ@�K�@�l�@��@�"�@���@�^5@�@���@��h@�x�@�hs@��`@���@��@�z�@�r�@�z�@�z�@��@�Z@��@��w@���@��P@�K�@�"�@�@���@��!@�$�@���@���@�p�@�G�@��@�V@�%@��@���@��@���@��D@� �@���@��P@�l�@�K�@�
=@��R@���@�-@��T@��h@�X@�&�@�%@���@���@�(�@�ƨ@�t�@�;d@�
=@���@�E�@��@��^@�G�@���@��j@��@��j@�j@�b@�t�@�dZ@�;d@�o@���@��\@�^5@�E�@��@��#@��-@�hs@�%@��j@��j@���@�A�@� �@��m@���@�t�@�S�@�"�@���@��H@��@���@��+@��@��^@���@��@�X@�V@��/@���@��D@�j@��@��@�|�@�S�@�C�@��@��y@���@�ff@�M�@��#@��7@�7L@��@��@��R@|�@u��@l9X@c"�@\(�@UV@N�R@H��@B��@;dZ@4�@.��@'\)@"-@9X@��@S�@A�@@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BDB
=B
=B	7B%BBBBBBBDB49B$�B{BhBhB&�B0!B-B,B1'B5?B=qB=qB?}BI�BZB\)B]/BiyBjBk�Bq�B�1B�bB�?B�#B�ZB�TB�`B�`B�fB�fB�`B�fB�fB�BBbB&�B<jB6FB+B6FBVBn�B�B�{B�Bx�Bu�Bz�B�1B�7B�7B�1B�+B�B}�B}�B��B�B�9B�9B�B��B�uB�oB��B��B�PB�%B{�Bp�BgmBM�B@�B8RB)�BbB��B�B�ZBɺB�?B�'B�!B��B}�BZB@�B0!B�BB
��BB
��B
�`B
��B
�B
�{B
�DB
�B
k�B
A�B
#�B
+B	��B	�`B	��B	��B	�!B	��B	�DB	� B	�B	�B	}�B	{�B	�B	�DB	�=B	�B	�B	~�B	z�B	v�B	q�B	cTB	S�B	H�B	@�B	6FB	)�B	!�B	�B	�B	oB	\B	
=B	B��B��B�B�B�B�B�B�fB�TB�BB�)B�B��B��B��BɺBƨBÖB��B�jB�RB�?B�'B�!B�B�B�B��B��B��B��B��B��B�uB�\B�DB�+B�B�B�B�B~�Bx�Bv�Bv�Bw�Bv�Bq�Bo�Bn�Bl�BjBhsBffBdZBcTBbNBcTBbNBaHB_;B_;BdZBe`BffBe`B`BB\)B[#B]/B^5B^5B^5B_;BcTBffBgmBiyBiyBhsBiyBl�Bq�Bu�Bw�Bz�B|�B}�B~�B~�B� B�B�B�B�B�%B�1B�bB��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�?B�RB�^B�qBĜBǮB��B��B��B�B�#B�HB�NB�TB�TB�`B�yB�B�B��B��B��B	B	
=B	VB	oB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	$�B	&�B	)�B	)�B	+B	+B	-B	1'B	49B	6FB	8RB	:^B	>wB	B�B	D�B	F�B	I�B	K�B	M�B	M�B	M�B	Q�B	T�B	VB	XB	XB	[#B	]/B	^5B	_;B	`BB	cTB	cTB	cTB	cTB	dZB	e`B	iyB	m�B	q�B	u�B	v�B	y�B	z�B	{�B	}�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�+B	�1B	�7B	�DB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�LB	�RB	�RB	�^B	�dB	�wB	��B	��B	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�;B	�HB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
%B
+B
	7B
VB
�B
 �B
(�B
0!B
49B
:^B
?}B
C�B
H�B
N�B
T�B
ZB
`BB
dZB
jB
o�B
r�B
v�B
x�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BLB
AB
HB	@B.BBBBBBBJB4=B$�B�BqBoB&�B0*B-B,B1.B5GB={B=}B?�BI�BZ(B\1B]7BiBj�Bk�Bq�B�9B�jB�DB�)B�`B�ZB�fB�hB�kB�kB�fB�mB�oB�B BiB&�B<pB6LB+B6NBVBn�B�B��B�Bx�Bu�Bz�B�7B�;B�;B�8B�5B� B}�B}�B��B�B�BB�DB�B��B�|B�xB��B��B�\B�+B{�Bp�BgxBM�B@�B8VB*BgB��B�B�cB��B�GB�/B�.B��B}�BZ(B@�B0*B�BB
� B"B
��B
�jB
��B
�
B
��B
�OB
�B
k�B
A�B
#�B
>B	��B	�oB	��B	��B	�6B	��B	�WB	�B	�B	�B	~
B	{�B	�0B	�[B	�QB	�.B	�(B	B	z�B	v�B	q�B	clB	TB	H�B	@�B	6\B	*B	!�B	�B	�B	�B	tB	
UB	*B��B��B�B�B�B�B��B�~B�oB�^B�EB�B�B��B��B��B��BñB��B��B�nB�^B�BB�=B�7B�)B�!B�B�B��B�B��B��B��B�}B�bB�JB�:B�=B�3B�#BBx�Bv�Bv�Bw�Bv�Bq�Bo�Bn�Bl�Bj�Bh�Bf�BdyBcsBbmBcvBbpBagB_[B_\BdyBe�Bf�Be�B`dB\IB[DB]PB^VB^WB^WB_\BcxBf�Bg�Bi�Bi�Bh�Bi�Bl�Bq�Bu�Bw�B{ B}B~BBB�"B�&B�+B�(B�&B�CB�QB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�"B�&B�=B�JB�VB�YB�nB�zB��BĸB��B��B��B�	B�B�=B�dB�lB�qB�pB�zB�B��B��B��B��B�	B	*B	
SB	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	$�B	'B	*B	*B	+B	+B	-%B	1>B	4PB	6]B	8jB	:vB	>�B	B�B	D�B	F�B	I�B	K�B	M�B	M�B	M�B	RB	UB	VB	X&B	X%B	[:B	]FB	^MB	_RB	`WB	cgB	ciB	chB	cjB	dpB	evB	i�B	m�B	q�B	u�B	v�B	y�B	z�B	{�B	~
B	B	B	B	B	�B	�"B	�,B	�4B	�@B	�HB	�LB	�YB	�jB	�uB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�)B	�<B	�GB	�XB	�ZB	�aB	�`B	�dB	�dB	�rB	�vB	��B	��B	��B	èB	ıB	ųB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�(B	�*B	�5B	�;B	�BB	�MB	�ZB	�fB	�qB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B
 B
 B
 B
 B
B
"B
-B
,B
.B
7B
:B
	GB
fB
�B
 �B
)B
03B
4JB
:lB
?�B
C�B
H�B
N�B
UB
Z-B
`RB
dgB
j�B
o�B
r�B
v�B
x�B
~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230510  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230510  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                