CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-08T05:01:01Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160708050101  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286_8897_128                   2C  D   APEX                            6531                            072314                          846 @׹ˈ'w1   @׹�q�@6["��`B�c(���S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@���A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D�  D�6fD��fD�� D���D�,�D���D�ɚD�  D�33D�vfDǶfD�3D�I�D�s3D�ɚD���D�0 D�ffD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�=q@�p�A�RA>�RA^�RA~�RA�(�A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�CC C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DGDz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DRGDRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��DtnDyt{D�qD�3�D���D��qD��>D�*>D��>D��D��qD�0�D�s�Dǳ�D��D�GD�p�D��D��>D�-qD�c�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�n�A�p�A�v�A�|�A�z�A�p�A�l�A�n�A�l�A�dZA��A���A��A��A��`A��`A��TA��`A��TA��TA��TA��/A���A���A�ĜA̲-A̙�A�|�A�ffA�+A˴9AɶFA� �AǗ�A��A�l�A�;dA���Ať�A�A�A�"�A��AĴ9A�|�A���A�\)AāA���A�{A\A�dZA�-A�^5A���A��A���A�|�A�E�A��A�G�A���A�?}A�JA�A���A�bNA��PA�^5A��A���A�A�A��!A�;dA�VA���A��\A���A�^5A��A��A�A�VA�G�A���A��mA�"�A�A�A��HA�t�A�-A�x�A��TA���A�hsA��\A�ZA��A�33A��FA�Q�A���A��;A�E�A�$�A�A�A��A��;A�1'A��RA�G�A�jA���A���A�I�A�5?A�A���A�-A��A~��A}�FA{�PAx�jAuXAo�;AjĜAg�FAe`BAd~�Abn�A`z�A_��A\�A[�TAXĜAVA�AV��AWO�AUx�AQ|�APE�AOƨAM�
AJ �AG%AE�PAD�\AB��AA�A@�RA@bA?C�A>ZA=�A<I�A;�PA:�HA9ƨA9oA7�;A6�A5dZA3��A2�A1dZA0�/A0-A.�9A-�7A,�yA,9XA*^5A(�RA(1A'K�A%"�A#\)A!VAS�A�yAQ�AjA��A9XA��A��A�9A�;A�RA�mA�7Ap�AO�A=qA&�A�!A��A~�A�TA�
A��A�A��A��A�PA��AĜA�uAr�AA�A�A1A��A	��A�Az�AjA9XA��AhsAVAC�A9XA�wA/A��A�DA �AƨA �u@���@���@��
@�t�@�V@��P@�ff@���@�D@�hs@��H@�?}@�1@�t�@�R@�V@�1'@�!@�-@��@��y@��#@�r�@�ȴ@���@�`B@�z�@���@�M�@�^5@�=q@�J@��@ٙ�@�X@�V@��/@�I�@�+@�E�@��@��@���@ա�@�G�@�I�@��H@ѩ�@�X@�9X@�dZ@���@�\)@�\)@�M�@�V@�~�@���@��H@��@��y@��@��y@θR@�=q@�@�&�@��@���@̣�@�r�@���@˕�@�\)@��@ʧ�@���@���@ȃ@�1@�n�@ř�@��@��@��@�9X@�
=@�@�@�bN@�S�@��H@��R@�$�@���@��@��`@���@��@���@��@��@��@���@��^@��-@�p�@���@�Ĝ@�9X@�33@��@���@��!@���@�n�@�{@��@��u@�bN@�1'@�I�@���@�t�@�t�@�dZ@�M�@�&�@���@���@�Ĝ@��@���@��w@��R@�@��@�;d@���@��^@���@��-@��T@��-@���@�X@�X@��@�V@��@�O�@�/@�/@��@��@���@�j@��m@�1'@�A�@�r�@��@�(�@���@�E�@�ff@�t�@�l�@�S�@�l�@�33@��@��@���@�7L@�7L@��7@��-@�?}@���@��`@��
@���@�v�@�E�@��@���@�{@�?}@���@���@��w@�t�@�
=@���@�ȴ@���@��R@�~�@�^5@�{@�p�@��@���@���@�I�@��@���@�K�@�;d@���@�n�@�=q@��#@��@��u@�9X@�b@���@�ƨ@�l�@��@�~�@�E�@���@���@���@��7@��@�p�@�hs@�hs@�O�@�/@��@��`@�9X@��w@�33@���@��y@��@�ȴ@��!@�~�@�-@�{@��-@�`B@�/@��/@���@�z�@�j@�Q�@�  @��
@�E�@��#@��@|�@tZ@h�`@d�/@Yhs@S@O�P@J�H@E�h@;dZ@2��@+�m@)�@%?}@ 1'@o@�y@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�~�A�n�A�p�A�v�A�|�A�z�A�p�A�l�A�n�A�l�A�dZA��A���A��A��A��`A��`A��TA��`A��TA��TA��TA��/A���A���A�ĜA̲-A̙�A�|�A�ffA�+A˴9AɶFA� �AǗ�A��A�l�A�;dA���Ať�A�A�A�"�A��AĴ9A�|�A���A�\)AāA���A�{A\A�dZA�-A�^5A���A��A���A�|�A�E�A��A�G�A���A�?}A�JA�A���A�bNA��PA�^5A��A���A�A�A��!A�;dA�VA���A��\A���A�^5A��A��A�A�VA�G�A���A��mA�"�A�A�A��HA�t�A�-A�x�A��TA���A�hsA��\A�ZA��A�33A��FA�Q�A���A��;A�E�A�$�A�A�A��A��;A�1'A��RA�G�A�jA���A���A�I�A�5?A�A���A�-A��A~��A}�FA{�PAx�jAuXAo�;AjĜAg�FAe`BAd~�Abn�A`z�A_��A\�A[�TAXĜAVA�AV��AWO�AUx�AQ|�APE�AOƨAM�
AJ �AG%AE�PAD�\AB��AA�A@�RA@bA?C�A>ZA=�A<I�A;�PA:�HA9ƨA9oA7�;A6�A5dZA3��A2�A1dZA0�/A0-A.�9A-�7A,�yA,9XA*^5A(�RA(1A'K�A%"�A#\)A!VAS�A�yAQ�AjA��A9XA��A��A�9A�;A�RA�mA�7Ap�AO�A=qA&�A�!A��A~�A�TA�
A��A�A��A��A�PA��AĜA�uAr�AA�A�A1A��A	��A�Az�AjA9XA��AhsAVAC�A9XA�wA/A��A�DA �AƨA �u@���@���@��
@�t�@�V@��P@�ff@���@�D@�hs@��H@�?}@�1@�t�@�R@�V@�1'@�!@�-@��@��y@��#@�r�@�ȴ@���@�`B@�z�@���@�M�@�^5@�=q@�J@��@ٙ�@�X@�V@��/@�I�@�+@�E�@��@��@���@ա�@�G�@�I�@��H@ѩ�@�X@�9X@�dZ@���@�\)@�\)@�M�@�V@�~�@���@��H@��@��y@��@��y@θR@�=q@�@�&�@��@���@̣�@�r�@���@˕�@�\)@��@ʧ�@���@���@ȃ@�1@�n�@ř�@��@��@��@�9X@�
=@�@�@�bN@�S�@��H@��R@�$�@���@��@��`@���@��@���@��@��@��@���@��^@��-@�p�@���@�Ĝ@�9X@�33@��@���@��!@���@�n�@�{@��@��u@�bN@�1'@�I�@���@�t�@�t�@�dZ@�M�@�&�@���@���@�Ĝ@��@���@��w@��R@�@��@�;d@���@��^@���@��-@��T@��-@���@�X@�X@��@�V@��@�O�@�/@�/@��@��@���@�j@��m@�1'@�A�@�r�@��@�(�@���@�E�@�ff@�t�@�l�@�S�@�l�@�33@��@��@���@�7L@�7L@��7@��-@�?}@���@��`@��
@���@�v�@�E�@��@���@�{@�?}@���@���@��w@�t�@�
=@���@�ȴ@���@��R@�~�@�^5@�{@�p�@��@���@���@�I�@��@���@�K�@�;d@���@�n�@�=q@��#@��@��u@�9X@�b@���@�ƨ@�l�@��@�~�@�E�@���@���@���@��7@��@�p�@�hs@�hs@�O�@�/@��@��`@�9X@��w@�33@���@��y@��@�ȴ@��!@�~�@�-@�{@��-@�`B@�/@��/@���@�z�@�j@�Q�@�  G�O�@�E�@��#@��@|�@tZ@h�`@d�/@Yhs@S@O�P@J�H@E�h@;dZ@2��@+�m@)�@%?}@ 1'@o@�y@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB

=B
	7B
	7B
1B
	7B
	7B
1B
1B
1B
1B
+B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
	7B
JB
hB
49B
J�B
N�B
R�B
S�B
W
B
YB
XB
YB
_;B
`BB
`BB
ffB
��B
�B
�
B
�ZB
�yB{B%�B1'B.B<jBO�B^5Bq�B�1B�B|�Bs�Bo�Bn�Bn�Bm�Bq�Bp�B}�B�%B�VB��B��B��B��B��B�B�B�'B�^B�LB�3B�'B�BƨBŢB�^B�B��B��B�PBy�BjBW
B]/Bt�Bx�Bw�Bx�B}�BhsB2-B
��B
�3B
t�B
Q�B
K�B
+B
hB
	7B
  B	�B	�B	��B	�?B	��B	�B	ffB	Q�B	B�B	=qB	6FB	)�B	�B��B�B�}B�B��B��B��B��B��B��B��B�7B� B��B�B��B�B�B�PB� BffBaHB]/B[#B^5BbNBbNB`BB^5B_;BaHBgmBk�Bq�Bw�B}�B�\B��B��B��B��B��B��B��B�?B�}B��BÖBȴBƨBǮBƨB�dB�!B��B�\B��B��B��B�-B�?B�LB�wB��B�wB�wB��BĜBĜBĜB��B�B�)B�5B�NB�mB�mB�mB�B�B�B�B�B�B�B�B�B�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�yB�sB�fB�TB�;B�/B�B�B�B��B��B��BȴBĜB�qB�RB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�-B�3B�?B�^B��BĜBŢBȴB��B��B��B��B��B�B�/B�BB�NB�fB�B�B��B	B	+B	
=B	PB	VB	\B	bB	hB	�B	�B	�B	�B	�B	 �B	!�B	#�B	&�B	+B	-B	-B	.B	.B	0!B	1'B	1'B	1'B	2-B	33B	1'B	0!B	=qB	=qB	:^B	8RB	5?B	6FB	9XB	>wB	@�B	E�B	G�B	G�B	G�B	I�B	J�B	K�B	L�B	O�B	O�B	O�B	O�B	Q�B	W
B	YB	[#B	`BB	aHB	aHB	aHB	aHB	aHB	`BB	^5B	^5B	_;B	_;B	cTB	e`B	e`B	ffB	e`B	e`B	e`B	ffB	ffB	ffB	hsB	k�B	m�B	k�B	gmB	cTB	e`B	ffB	gmB	m�B	o�B	q�B	t�B	x�B	y�B	{�B	|�B	~�B	�B	�%B	�1B	�DB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�9B	�?B	�LB	�FB	�9B	�3B	�3B	�'B	�9B	�dB	�wB	�qB	�jB	�jB	�XB	�FB	�FB	�FB	�FB	�XB	�qB	�jB	�jB	�qB	�wB	�}B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�NB	�NB	�HB	�HB	�NB	�NB	�HB	�NB	�ZB	�ZB	�fB	�sB	�sB	�yB	�yB	�yB	�yB	�sB	�sB	�yB	�B	��B
B
bB
�B
 �B
%�B
-B
1'B
49B
;dB
@�B
D�B
M�B
S�B
VB
YB
^5B
cTB
gmB
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B

LB
	EB
	GB
CB
	GB
	EB
CB
AB
AB
AB
:B
.B
(B
!B
%B
B
!B
!B
)B
!B
)B
*B
)B
*B
*B
.B
-B
BB
AB
	FB
XB
uB
4IB
J�B
N�B
R�B
T	B
WB
Y#B
XB
Y#B
_EB
`MB
`MB
frB
��B
�"B
�B
�bB
�B�B%�B11B.B<uBO�B^;Bq�B�7B�B|�Bs�Bo�Bn�Bn�Bm�Bq�Bp�B}�B�,B�`B��B��B��B��B��B�B�B�/B�eB�SB�7B�/B�$BƮBũB�eB�B��B��B�XBy�Bj�BWB]4Bt�Bx�Bw�Bx�B}�Bh{B25B
��B
�=B
t�B
Q�B
K�B
+B
wB
	FB
 B	�B	�"B	��B	�PB	��B	�.B	f|B	RB	B�B	=�B	6\B	*B	�B�B�8B��B�:B�B��B��B��B��B��B��B�WB� B��B�8B��B�1B�4B�oB�"Bf�BaiB]PB[CB^XBboBbmB`cB^TB_^BajBg�Bk�Bq�Bw�B~B�}B��B��B��B��B��B��B�B�ZB��B��BóB��B��B��B��B��B�?B��B�|B��B��B��B�JB�_B�iB��B��B��B��B��BĹBĻBĻB��B�!B�DB�PB�jB�B�B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�B�pB�VB�IB�:B�,B� B�B��B��B��BĹB��B�nB�XB�JB�CB�6B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�*B�=B�AB�OB�IB�QB�[B�zB��BĸB��B��B��B��B��B� B�B�2B�KB�[B�jB�B�B�B��B	%B	DB	
SB	jB	oB	uB	|B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	' B	+B	-%B	-$B	.-B	.,B	07B	1>B	1>B	1<B	2CB	3KB	1@B	08B	=�B	=�B	:sB	8jB	5VB	6]B	9mB	>�B	@�B	E�B	G�B	G�B	G�B	I�B	J�B	K�B	L�B	O�B	O�B	O�B	O�B	RB	WB	Y/B	[8B	`VB	a]B	a]B	a`B	a^B	a^B	`XB	^KB	^MB	_PB	_QB	ciB	etB	evB	fzB	evB	etB	evB	f|B	f|B	fzB	h�B	k�B	m�B	k�B	g�B	ckB	evB	f{B	g�B	m�B	o�B	q�B	t�B	x�B	y�B	{�B	}B	B	�3B	�7B	�DB	�YB	�fB	�pB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�>B	�KB	�TB	�_B	�XB	�LB	�DB	�GB	�;B	�KB	�vB	��B	��B	�|B	�}B	�kB	�YB	�XB	�YB	�ZB	�jB	��B	�}B	�}B	��B	��B	��B	êB	çB	ĮB	įB	įB	ŵB	ƹB	ƺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�!B	�"B	�B	�(B	�5B	�4B	�5B	�AB	�GB	�MB	�MB	�LB	�LB	�OB	�NB	�NB	�OB	�LB	�MB	�\B	�_B	�_B	�\B	�ZB	�`B	�bB	�[B	�]B	�lB	�jB	�wB	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�B	��B
#B
sB
�B
 �B
%�B
-B
17B
4JB
;uB
@�B
D�B
M�B
TB
VB
Y&B
^EB
ccB
g|B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160708050101    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160708050101  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160708050101  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                