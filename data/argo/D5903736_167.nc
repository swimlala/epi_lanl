CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]t   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qL   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181121041154  20190604094024  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��/�4�1   @��0#�@3��x����d��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A��A   A@  A^ffA�  A�  A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�)D��D�1�D�� D�ϮD���D��D���D��qD��)D�D)D��)D���D�qD�1�Dڌ{D��RD��D�$)D�`R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�p�A Q�A�RA>�RA]�A~�RA�\)A��\A��\A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=B��
B���B��
B��
B��
B��
B�=pBǣ�B��
Bϣ�B��
B��
B��
B��
B��
B��
B�=pB��B��
B��
B��
B��
C�C��C�C�C	�C�C�C�C�C�C�C�C�C�C�C C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtt{Dy�
D�	HD�/]D��qD��D��D�D��gD���D���D�A�D���D��4D��D�/]Dډ�D���D�� D�!�D�]�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��/A��HA��A���A���A��;A���A��
A��A���A���A���A���A�A�%A�A���A�  A���A���A���A�  A�A�%A�1A�
=A�JA�JA�VA�bA�oA�oA�{A��A�+A�/A�1'A�=qA�XA�|�Aؕ�Aإ�A׸RA֧�A�ƨA�"�A��A���A��/A��#Aɴ9A��mA�(�A�$�A��mA�A�bNA�(�A��A�Q�A�ffA�\)A��
A���A�G�A��
A�v�A��A�?}A�z�A��A�+A��uA�%A��A��A��A��`A���A�I�A�VA�-A�O�A��A�ĜA�ƨA���A�JA��mA���A���A�x�A�K�A���A���A�"�A�l�A�JA���A�JA�ffA�VA���A�1'A���A��7A�"�A���A�Q�A���A�A�jA��
A�XA~5?A|�\A{��Az�AyAwoAt��AsƨAsG�Arn�Aq�^Apr�AoƨAnI�Alv�Aj�Ai�Ahr�AfAd��A`�A^�`A]�A]%A\�AZ$�AY�AXE�AWx�AT��AS�7ARAQ�AP �AN�AM�PAK��AI�FAHbNAG�;AGO�AD�AC"�AAx�A@v�A??}A>�A=?}A<^5A;�;A:�yA9��A81A7oA5�^A4�jA3��A2bNA1+A/�A-t�A,�+A+�A)t�A(�RA( �A'dZA%�A$��A#"�A!��A!&�A�A;dA�AȴA~�A�wAt�A�+A�yA�A"�A(�AO�A(�A��AVA�+A(�Ax�AXA33A
ZA	��Ar�Ax�A�uA?}A�;A��A~�A1'A��A�A �\@�+@��j@���@�X@��@���@�1'@�\)@��@@���@�j@��@�P@�M�@�@�C�@�!@�z�@�ƨ@��@�`B@��@��D@�l�@ޏ\@��@���@۾w@�5?@�`B@�V@؋D@�9X@���@׾w@ו�@��y@թ�@�A�@���@�;d@�~�@���@д9@��y@�5?@���@�?}@�Q�@ʰ!@�5?@ɺ^@ə�@�x�@Ȭ@�Q�@ǥ�@�dZ@Ɵ�@ŉ7@ēu@� �@���@�+@�=q@�X@�z�@��;@��@�C�@�=q@�`B@���@��m@�dZ@��y@��-@�Ĝ@��@��F@��@��@�ff@���@�&�@�A�@���@�+@���@��H@���@��R@��+@�@��@���@��@��7@�O�@���@�bN@��@�C�@��@��\@���@���@���@�r�@��@��@���@�l�@��@�M�@�G�@��D@�Q�@�z�@�/@�&�@���@�1'@��@���@��@�+@�~�@�V@��@��@��@�?}@�V@���@�Z@���@�\)@��y@�ȴ@���@���@�~�@�M�@��@��@�O�@�&�@���@���@��@�Z@�1'@� �@�b@��@���@�|�@�\)@�@���@��+@�n�@�V@�$�@���@��-@��7@�X@�7L@��@���@�Q�@�(�@�(�@�  @��
@��@�l�@�33@�o@���@�ȴ@��!@���@�M�@�$�@�@�hs@�`B@�%@�z�@�Z@�A�@��@���@��
@��;@��m@��F@�t�@���@�ȴ@�ȴ@��\@�ff@�5?@�$�@���@��-@�x�@�X@�/@���@��u@�9X@�1'@�(�@��@���@�ƨ@�C�@�+@�+@�"�@���@�$�@��@��-@��7@�hs@�V@��D@�1@��w@��P@�"�@���@���@�^5@�{@���@�7L@��@�9X@���@��;@��F@�l�@���@�n�@�V@�5?@���@���@��7@�`B@�V@���@���@�z�@�9X@��m@���@��@�ȴ@��f@x?�@p@g�@`�/@X��@S�V@L�$@H2�@CMj@<��@5��@.1�@)�n@$w�@�@�@�,@:*@҉1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A��/A��/A��HA��A���A���A��;A���A��
A��A���A���A���A���A�A�%A�A���A�  A���A���A���A�  A�A�%A�1A�
=A�JA�JA�VA�bA�oA�oA�{A��A�+A�/A�1'A�=qA�XA�|�Aؕ�Aإ�A׸RA֧�A�ƨA�"�A��A���A��/A��#Aɴ9A��mA�(�A�$�A��mA�A�bNA�(�A��A�Q�A�ffA�\)A��
A���A�G�A��
A�v�A��A�?}A�z�A��A�+A��uA�%A��A��A��A��`A���A�I�A�VA�-A�O�A��A�ĜA�ƨA���A�JA��mA���A���A�x�A�K�A���A���A�"�A�l�A�JA���A�JA�ffA�VA���A�1'A���A��7A�"�A���A�Q�A���A�A�jA��
A�XA~5?A|�\A{��Az�AyAwoAt��AsƨAsG�Arn�Aq�^Apr�AoƨAnI�Alv�Aj�Ai�Ahr�AfAd��A`�A^�`A]�A]%A\�AZ$�AY�AXE�AWx�AT��AS�7ARAQ�AP �AN�AM�PAK��AI�FAHbNAG�;AGO�AD�AC"�AAx�A@v�A??}A>�A=?}A<^5A;�;A:�yA9��A81A7oA5�^A4�jA3��A2bNA1+A/�A-t�A,�+A+�A)t�A(�RA( �A'dZA%�A$��A#"�A!��A!&�A�A;dA�AȴA~�A�wAt�A�+A�yA�A"�A(�AO�A(�A��AVA�+A(�Ax�AXA33A
ZA	��Ar�Ax�A�uA?}A�;A��A~�A1'A��A�A �\@�+@��j@���@�X@��@���@�1'@�\)@��@@���@�j@��@�P@�M�@�@�C�@�!@�z�@�ƨ@��@�`B@��@��D@�l�@ޏ\@��@���@۾w@�5?@�`B@�V@؋D@�9X@���@׾w@ו�@��y@թ�@�A�@���@�;d@�~�@���@д9@��y@�5?@���@�?}@�Q�@ʰ!@�5?@ɺ^@ə�@�x�@Ȭ@�Q�@ǥ�@�dZ@Ɵ�@ŉ7@ēu@� �@���@�+@�=q@�X@�z�@��;@��@�C�@�=q@�`B@���@��m@�dZ@��y@��-@�Ĝ@��@��F@��@��@�ff@���@�&�@�A�@���@�+@���@��H@���@��R@��+@�@��@���@��@��7@�O�@���@�bN@��@�C�@��@��\@���@���@���@�r�@��@��@���@�l�@��@�M�@�G�@��D@�Q�@�z�@�/@�&�@���@�1'@��@���@��@�+@�~�@�V@��@��@��@�?}@�V@���@�Z@���@�\)@��y@�ȴ@���@���@�~�@�M�@��@��@�O�@�&�@���@���@��@�Z@�1'@� �@�b@��@���@�|�@�\)@�@���@��+@�n�@�V@�$�@���@��-@��7@�X@�7L@��@���@�Q�@�(�@�(�@�  @��
@��@�l�@�33@�o@���@�ȴ@��!@���@�M�@�$�@�@�hs@�`B@�%@�z�@�Z@�A�@��@���@��
@��;@��m@��F@�t�@���@�ȴ@�ȴ@��\@�ff@�5?@�$�@���@��-@�x�@�X@�/@���@��u@�9X@�1'@�(�@��@���@�ƨ@�C�@�+@�+@�"�@���@�$�@��@��-@��7@�hs@�V@��D@�1@��w@��P@�"�@���@���@�^5@�{@���@�7L@��@�9X@���@��;@��F@�l�@���@�n�@�V@�5?@���@���@��7@�`B@�V@���@���@�z�@�9X@��m@���@��G�O�@��f@x?�@p@g�@`�/@X��@S�V@L�$@H2�@CMj@<��@5��@.1�@)�n@$w�@�@�@�,@:*@҉1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
y�B
z�B
z�B
y�B
z�B
z�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
�B
�1B
�7B
�=B
�VB
��B
B
�5BhB�RB�B��B33BVBffB�+B�bB�bB�Bw�B~�B�%B�DB��B��BƨB��B��B�B�BB�TB�fB�ZB�HB�)B�B��B��BĜB�'B��B�%Bv�Bn�BcTB]/BS�BB�BA�B8RB�B�B�;B�
B�fB�HB�B�B��B�qB�-B��B��B�Br�Bk�BdZB[#BL�B:^B1'B+B%�B�B�BhB
��B
�yB
��B
�wB
��B
��B
�=B
�B
v�B
k�B
]/B
L�B
F�B
B�B
;dB
5?B
-B
&�B
�B
VB
B	��B	�B	�;B	��B	�}B	�?B	�!B	��B	��B	��B	�JB	�%B	�B	v�B	r�B	k�B	e`B	^5B	XB	O�B	E�B	:^B	49B	2-B	1'B	&�B	�B	�B	bB	
=B	B��B��B��B�B�B�`B�sB�sB�HB�B��BɺBÖB�dB�LB�-B�B�B��B��B��B��B��B�uB�\B�=B�%B�B{�Bx�Bw�Bv�Bt�Bq�Bn�Bk�BhsBe`BdZBcTB`BB^5B]/B^5B`BBl�Bl�BhsBaHBdZBhsBffBgmBgmBhsBhsBiyBiyBjBo�Bm�Bl�Bq�Bx�By�Bz�B{�B}�B�+B�VB��B��B��B��B��B��B��B��B��B��B��B��B�'B�3B�3B�FB�LB�XB�^B�dB�jB�qB�wB�wB�wB�wB�}B�}B�wB�wB��BBÖBÖBÖBĜBŢBĜBĜBȴB��B��B��B��B��B��B��B��B�
B�B�/B�5B�5B�BB�TB�mB�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B	1B		7B	
=B	DB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	+B	,B	,B	-B	.B	/B	2-B	8RB	=qB	?}B	A�B	F�B	I�B	J�B	K�B	O�B	Q�B	S�B	VB	YB	\)B	]/B	ffB	o�B	q�B	r�B	u�B	v�B	v�B	w�B	w�B	x�B	y�B	y�B	z�B	{�B	~�B	�B	�B	�B	�%B	�7B	�DB	�DB	�PB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�?B	�FB	�XB	�^B	�dB	�qB	�wB	�}B	�wB	�}B	��B	ĜB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
JB
JB
PB
VB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
{B

B
�B
(�B
0�B
8B
=�B
DB
I�B
L�B
Q�B
W�B
]B
c:B
f�B
kkB
o�B
shB
w�B
{�B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B
uIB
uGB
uIB
uHB
uEB
uEB
uIB
vLB
vMB
vLB
uIB
vMB
vMB
uHB
vMB
vNB
uGB
uGB
uDB
uJB
uHB
uGB
uIB
uFB
vMB
vMB
wTB
wTB
wQB
wTB
xZB
xZB
x\B
ybB
}vB
��B
��B
��B
��B
�!B
��B
ٜB�B��B��B�RB.�BQcBa�B��B��B��B��Bs(BzUB��B��B�B�WB�B�KB�SB�cBۡBޱB��B߸BܢB׃B�kB�MB�2B��B��B��B��Br+Bi�B^�BX�BO[B=�B<�B3�BB��BڢB�rB��BܰBԁB�kB�0B��B��B�BB�B��BnBf�B_�BV�BH<B5�B,�B&nB!OB/BB�B
�UB
��B
�mB
��B
�cB
��B
��B
|}B
r?B
f�B
X�B
HDB
B!B
>	B
6�B
0�B
(�B
"bB
B
	�B	��B	�TB	�B	ڸB	�wB	��B	��B	��B	�{B	�JB	�B	��B	��B	|�B	rLB	n/B	gB	`�B	Y�B	S�B	K`B	A#B	5�B	/�B	-�B	,�B	"pB	;B	B	�B	�B��B��B�pB�[B�>B�B��B��B��B��BըB�kB�DB�B��B��B��B��B��B��B�zB�\B�BB� B�B��B��B��B|�BwpBteBs]BrWBpNBm7Bj$BgBdB`�B_�B^�B[�BY�BX�BY�B[�BhBhBdB\�B_�Bd Ba�Bb�Bb�BdBdBe	BeBfBk0Bi BhBm9BtfBujBvoBwwBy�B��B��B�B�B�"B�.B�CB�HB�JB�`B�cB�cB�wB�|B��B��B��B��B��B��B��B��B��B� B�	B�B�B�B�B�B�B�B�B�B�&B�%B�)B�+B�/B�*B�)B�AB�bB�hB�dB�eB�zB΂BψBЋBҗBԢBغB��B��B��B��B��B�B�(B�'B�/B�BB�XB�cB�|B��B��B��B��B	�B	�B	�B	�B	�B	�B		�B	B	(B	8B	;B	DB	DB	@B	CB	FB	 jB	"wB	&�B	'�B	'�B	(�B	)�B	*�B	-�B	3�B	8�B	;B	=B	B5B	ECB	FLB	GSB	KlB	MvB	O�B	Q�B	T�B	W�B	X�B	a�B	k'B	m3B	n=B	qLB	rUB	rRB	sXB	s[B	t`B	uiB	ujB	vkB	wsB	z�B	~�B	~�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�"B	�(B	�@B	�EB	�VB	�dB	�_B	�fB	�mB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�#B	�0B	�8B	�3B	�EB	�PB	�TB	�XB	�]B	�eB	�eB	�rB	�}B	ЄB	щB	ғB	ҖB	ҔB	ӚB	ԛB	բB	գB	״B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�$B	�-B	�,B	�,B	�,B	�-B	�-B	�8B	�>B	�?B	�=B	�NB	�PB	�TB	�UB	�[B	�^B	�aB	�uB	�zB	�yB	�~B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
	B
$xB
,yB
3�B
9GB
?�B
E'B
HnB
M=B
SzB
X�B
^�B
bB
f�B
kuB
n�B
sB
wjB
{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.004(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940242019060409402420190604094024  AO  ARCAADJP                                                                    20181121041154    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041154  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041154  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094024  IP                  G�O�G�O�G�O�                