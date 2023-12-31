CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:53Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181121041153  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��Л��1   @���?%�t@2�~��"��d�ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DsٚDy��D��D�R�D�z�D��qD���D�4)D�~�D�ٚD� D�{D���Dǻ�D��qD�'\Dڈ D�� D� D�P�D�RD�
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`zBg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5��C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dt{D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds�{Dy�{D�  D�PRD�xRD���D��D�1�D�|)D��D�qD��D��]DǸ�D���D�$�DڅqD��qD�qD�NgD�}�D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�`BA�bNA�dZA�bNA�dZA�ffA�dZA�^5A�M�A�O�A�9XA�oAݰ!A�9XAܼjA�t�A��A��;A�A۩�A�9XAڡ�AًDA���A�Q�A��;A��Aէ�Aԩ�A��mA�bA�JA��A�n�A˃AɃA�A�ȴA�~�A�9XA���A�5?A�-A�oA���A�$�A�p�A��HA��yA�XA�=qA���A�(�A� �A�E�A�+A��9A�5?A���A��A�A�A�
=A�l�A��TA�Q�A��A�+A��yA���A���A��#A�"�A���A���A�ƨA�=qA�`BA�  A���A��A�M�A�jA�A�Q�A�r�A�"�A��A���A��A�t�A�  A��A���A���A�\)A�VA��A��FA�7LA�bA���A���A�A�G�A�%A�VA��A��A&�A~VA|z�Axn�Aux�Atr�AsG�Ar �Ap�An�uAl�!Ak\)Aj~�Ai��Ahn�Ag+Ae��AcG�A`�A]ƨA\�AZ�HAX(�AW"�AUO�AT=qARVAQ��AP�AO�^AO"�AN  ALv�AKhsAJ�9AJjAJ5?AH�AGK�AE��AD�AC�-AB�A?�#A=/A:��A8��A6�A3�A2�A2^5A0v�A/�A.z�A-A-
=A,ffA+ƨA*ĜA)��A&�A%XA$v�A#��A!��A ��At�Ar�A5?A$�A1AAt�AA��A��An�A�A33Ar�AAVA�A�AI�A  AhsA�A�HA��A"�A��A�yAbA�A	�7A^5A�jA��AG�A�\A �yA n�@�dZ@��@���@���@�"�@��@��@�Z@�I�@��F@�l�@��@�O�@�I�@�{@�w@���@�M�@�bN@�K�@���@�hs@�@��@�-@��@�Q�@㝲@�7@��u@� �@�
=@�V@��@ڰ!@���@��
@�5?@���@�  @�+@���@�x�@Гu@Η�@�-@̬@ˮ@Ɂ@�hs@ēu@�(�@�(�@�(�@��;@�(�@��;@� �@ÍP@�~�@�ff@�~�@�=q@�7L@���@��y@�=q@��@���@��@��`@���@�bN@�(�@�1@��
@�;d@�^5@��@��u@��@�+@��@��R@�$�@�x�@��h@��@���@�+@��@�X@�7L@���@�Ĝ@��@���@�33@�+@�@�v�@���@�X@��9@�I�@� �@��@��@�o@��R@�M�@��@�@��@���@�b@��
@��F@��F@���@�@���@�-@�@��T@���@���@�`B@��@�Z@��@�  @���@�l�@�
=@��!@�v�@�V@�{@��#@���@��@���@�j@�1@��@�ƨ@��@�K�@�33@�@���@�v�@�^5@�=q@�J@��^@��7@�O�@��@��9@��@�r�@�1'@���@��
@��F@���@�K�@�"�@���@�-@���@���@��7@�`B@�&�@��/@�Z@�r�@�Q�@�9X@���@��P@��@��H@���@�-@��T@���@�X@���@��@�b@��@���@�\)@�+@�@���@��!@���@�V@�-@�-@�{@��^@��@��7@�?}@���@�Z@�1@�1'@�Ĝ@�1'@�dZ@���@��T@���@��^@��#@���@���@�X@�%@���@��@�1'@���@��;@���@���@�t�@�dZ@�33@��R@�v�@�E�@�5?@�$�@���@���@���@�&�@���@��@�9X@� �@�  @���@�ƨ@���@�C�@���@��H@�ȴ@���@�ff@�E�@�5?@��@���@��7@�hs@�O�@�V@�j@� �@��@���@��w@�|�@��@��@���@y=�@n� @e[W@[�P@U#�@Ma�@D�P@?
=@8�@2��@-*0@(�@$��@ ��@a@��@m�@��@�e@M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�`BA�`BA�bNA�dZA�bNA�dZA�ffA�dZA�^5A�M�A�O�A�9XA�oAݰ!A�9XAܼjA�t�A��A��;A�A۩�A�9XAڡ�AًDA���A�Q�A��;A��Aէ�Aԩ�A��mA�bA�JA��A�n�A˃AɃA�A�ȴA�~�A�9XA���A�5?A�-A�oA���A�$�A�p�A��HA��yA�XA�=qA���A�(�A� �A�E�A�+A��9A�5?A���A��A�A�A�
=A�l�A��TA�Q�A��A�+A��yA���A���A��#A�"�A���A���A�ƨA�=qA�`BA�  A���A��A�M�A�jA�A�Q�A�r�A�"�A��A���A��A�t�A�  A��A���A���A�\)A�VA��A��FA�7LA�bA���A���A�A�G�A�%A�VA��A��A&�A~VA|z�Axn�Aux�Atr�AsG�Ar �Ap�An�uAl�!Ak\)Aj~�Ai��Ahn�Ag+Ae��AcG�A`�A]ƨA\�AZ�HAX(�AW"�AUO�AT=qARVAQ��AP�AO�^AO"�AN  ALv�AKhsAJ�9AJjAJ5?AH�AGK�AE��AD�AC�-AB�A?�#A=/A:��A8��A6�A3�A2�A2^5A0v�A/�A.z�A-A-
=A,ffA+ƨA*ĜA)��A&�A%XA$v�A#��A!��A ��At�Ar�A5?A$�A1AAt�AA��A��An�A�A33Ar�AAVA�A�AI�A  AhsA�A�HA��A"�A��A�yAbA�A	�7A^5A�jA��AG�A�\A �yA n�@�dZ@��@���@���@�"�@��@��@�Z@�I�@��F@�l�@��@�O�@�I�@�{@�w@���@�M�@�bN@�K�@���@�hs@�@��@�-@��@�Q�@㝲@�7@��u@� �@�
=@�V@��@ڰ!@���@��
@�5?@���@�  @�+@���@�x�@Гu@Η�@�-@̬@ˮ@Ɂ@�hs@ēu@�(�@�(�@�(�@��;@�(�@��;@� �@ÍP@�~�@�ff@�~�@�=q@�7L@���@��y@�=q@��@���@��@��`@���@�bN@�(�@�1@��
@�;d@�^5@��@��u@��@�+@��@��R@�$�@�x�@��h@��@���@�+@��@�X@�7L@���@�Ĝ@��@���@�33@�+@�@�v�@���@�X@��9@�I�@� �@��@��@�o@��R@�M�@��@�@��@���@�b@��
@��F@��F@���@�@���@�-@�@��T@���@���@�`B@��@�Z@��@�  @���@�l�@�
=@��!@�v�@�V@�{@��#@���@��@���@�j@�1@��@�ƨ@��@�K�@�33@�@���@�v�@�^5@�=q@�J@��^@��7@�O�@��@��9@��@�r�@�1'@���@��
@��F@���@�K�@�"�@���@�-@���@���@��7@�`B@�&�@��/@�Z@�r�@�Q�@�9X@���@��P@��@��H@���@�-@��T@���@�X@���@��@�b@��@���@�\)@�+@�@���@��!@���@�V@�-@�-@�{@��^@��@��7@�?}@���@�Z@�1@�1'@�Ĝ@�1'@�dZ@���@��T@���@��^@��#@���@���@�X@�%@���@��@�1'@���@��;@���@���@�t�@�dZ@�33@��R@�v�@�E�@�5?@�$�@���@���@���@�&�@���@��@�9X@� �@�  @���@�ƨ@���@�C�@���@��H@�ȴ@���@�ff@�E�@�5?@��@���@��7@�hs@�O�@�V@�j@� �@��@���@��w@�|�@��G�O�@���@y=�@n� @e[W@[�P@U#�@Ma�@D�P@?
=@8�@2��@-*0@(�@$��@ ��@a@��@m�@��@�e@M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��BB�B,B.B2-B:^B@�BC�B=qB0!B"�B �B�B�B�BhBhBoBuB �B(�B6FBYB�oB��B��B��B�Bv�By�B�B�=B��B��B��B��B��B��B��B�B�5B�TB�BB�BB�BB�BB�5B�BƨB��BƨB�wB�-B��Bz�BbNBM�BC�B]/BL�BA�B=qB33B2-B.B-B0!B'�B�B��B�fBȴB�LB��B��B��B��B�Bl�BbNBZBO�BH�BA�B2-BDBBB
��B
��B
�B
�TB
ƨB
�FB
��B
��B
�{B
�PB
~�B
cTB
O�B
G�B
?}B
6FB
(�B
�B
hB
1B
B	��B	�B	�sB	�5B	��B	�RB	��B	��B	�oB	�B	y�B	o�B	hsB	]/B	VB	N�B	G�B	B�B	9XB	/B	+B	%�B	#�B	 �B	�B	oB	PB	%B	B��B�B�mB�BB��B��BÖBŢBÖB�}B�wB�dB�?B�B�B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�7B�B�B�B�B|�Bu�Bt�Bs�Bq�Bo�Bk�BhsBe`BdZBcTBaHB^5B[#BYBYBXBXBW
BYBYBZB^5Bv�B�1B�DB�\B�uB�{B��B��B�{B�{B�uB�uB��B��B��B��B�B�'B�!B�'B�-B�3B�LB�dB��B��B��BBB��BÖBĜBBÖBĜBĜBÖBBBÖBŢBÖB�wB�FB�XB�^B�^B�jB�jBÖB��B��B�B�B�)B�;B�HB�NB�NB�NB�ZB�yB�B�B�B�B��B��B��B��B	  B	B	+B	
=B	VB	\B	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	(�B	/B	1'B	1'B	0!B	33B	8RB	;dB	@�B	B�B	D�B	D�B	G�B	H�B	J�B	L�B	M�B	M�B	N�B	N�B	P�B	P�B	R�B	VB	ZB	^5B	_;B	bNB	bNB	cTB	cTB	dZB	ffB	iyB	k�B	m�B	n�B	o�B	r�B	w�B	y�B	{�B	{�B	|�B	~�B	� B	� B	�B	�+B	�DB	�JB	�JB	�\B	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�FB	�LB	�RB	�XB	�dB	�dB	�dB	�}B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�NB	�TB	�mB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
PB
VB
VB
VB
\B
bB
bB
bB
hB
hB
hB
hB
{B
B
 �B
(�B
2-B
<B
C�B
I�B
N�B
VB
ZkB
]IB
bB
e�B
jB
m�B
r-B
vB
z�B
~�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B�B��B�BbB(�B*�B.�B7B=@B@WB:,B,�B�B�BhB^BGB'B'B.B1B�B%�B3BU�B�,B�RB�YB�EB�Bs�Bv�B��B��B�mB��B�=BȅB͛BϮBѾB��B��B�B�B�B��B� B��B��B�eB�zB�hB�4B��B��Bw�B_BJ�B@UG�O�BI�B>NB:2B/�B.�B*�B)�B,�B$�BhB��B�'B�yB�B��B��B��B�oB�BiUB_BV�BL�BE}B>UB.�BB �B
��B
��B
��B
�hB
�!B
�uB
�B
��B
�wB
�KB
�$B
{�B
`&B
L�B
D|B
<OB
3B
%�B
�B
=B
B	��B	��B	�B	�IB	�	B	ȜB	�'B	��B	��B	�DB	��B	v�B	lvB	eLB	ZB	R�B	K�B	D�B	?cB	60B	+�B	'�B	"�B	 �B	�B	gB	FB	
-B	B��B��B�B�HB�B��BǞB�tBB�tB�ZB�TB�@B�B��B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�dB�GB�B��B��B�B}�By�Br�Bq�Bp�Bn�Bl�BhfBeXBbCBa;B`8B^,B[BXBU�BU�BT�BT�BS�BU�BU�BWB[Bs�B�B�'B�=B�YB�]B�cB�dB�bB�[B�XB�VB�`B�zB��B��B��B�
B�B�
B�B�B�+B�FB�nB�jB�iB�tB�oB�jB�xB�}B�qB�yB�~B�~B�|B�qB�rB�zBB�wB�XB�)B�:B�?B�BB�LB�OB�yBʱB��B��B��B�B�B�.B�0B�.B�0B�<B�XB�eB�rB�B�B�B��B��B��B��B��B	B	!B	9B	>B	=B	@B	NB	]B	lB	|B	�B	�B	�B	�B	�B	�B	"�B	%�B	+�B	.B	.B	-B	0B	52B	8DB	=eB	?oB	A}B	A|B	D�B	E�B	G�B	I�B	J�B	J�B	K�B	K�B	M�B	M�B	O�B	R�B	WB	[B	\B	_(B	_-B	`3B	`7B	a:B	cHB	fYB	hbB	jqB	kxB	l~B	o�B	t�B	v�B	x�B	x�B	y�B	{�B	|�B	|�B	�B	�B	�B	�)B	�(B	�9B	�AB	�>B	�KB	�YB	�[B	�gB	�kB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�'B	�/B	�5B	�AB	�>B	�BB	�YB	�hB	�oB	�uB	�~B	ŐB	ǠB	ǠB	ɫB	˲B	̾B	̽B	̼B	̼B	̼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�"B	�'B	�0B	�HB	�kB	�^B	�]B	�ZB	�UB	�eB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
!B
	%B
	#B
	)B

/B
0B
1B
5B
8B
@B
<B
;B
EB
EB
DB
DG�O�B
�B
�B
%iB
/B
8�B
@rB
F�B
K�B
R�B
WHB
Z'B
^�B
b�B
f�B
j�B
oB
r�B
w�B
{�B
�B
��1111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.003(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041153    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041153  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041153  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                