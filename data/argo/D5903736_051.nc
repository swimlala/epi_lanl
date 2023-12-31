CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:33Z AOML 3.0 creation; 2016-05-31T19:14:33Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230533  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               3A   AO  4051_7090_051                   2C  D   APEX                            5368                            041511                          846 @ִ͏ 1   @ִZ��@4���`A��e��"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    3A   B   B   @���@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX��B^ffBh  Bo��Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DuFfDy��D� D�P D��fD�ɚD�	�D�9�D�s3D��fD� D�VfD�#3D��fD���D�Y�Dڌ�D�� D��D�FfD� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
>@���@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BXz�B^zBg�BoG�Bw�B�B�
=B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��DuAGDy��D�qD�MqD���D��D�D�7D�p�D���D�qD�S�D� �D���D��>D�WDڊ>D�qD�
>D�C�D�qD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AݍPAݏ\AݍPAݏ\AݓuAݕ�Aݙ�Aݙ�Aݛ�Aݟ�Aݡ�Aݟ�Aݟ�Aݡ�A݃A�dZA�?}A�;dA�33A�+A��A��A��/Aܺ^A܅A��/AғuAɡ�A�5?A�A�A�33A�r�A��9A��A�$�A��jA�~�A�Q�A���A�-A�r�A�Q�A�  A� �A�A�\)A�I�A��HA��A�33A��A�K�A�~�A�
=A��^A�-A�I�A���A�E�A�9XA��hA�JA�t�A���A���A�n�A���A�M�A���A�=qA��A��jA�33A��\A��#A��A�VA���A���A��A���A�ĜA��A�|�A��uA�hsA��TA��PA�ĜA��DA�$�A���A��wA���A��!A�^5A��uA|�DA{S�Ax��AwoAv�9AtbNAq��AqƨAq
=ApĜApv�Ao�FAn��AlȴAh�/Ae�
A_��A]oA[�AZ{AY�PAYx�AYl�AY/AV9XAP=qAN�AM�AL-AK�-AJ��AG��AG
=AE��AC��AA�FAA+A@ �A<�RA8ffA6�A5t�A4��A3�A2��A21A1"�A0n�A/ƨA.�yA-��A-oA*��A(�A&z�A$��A$�\A#��A"�A"A�A!oA ȴA ��A �A {A��AI�A �A"�AJA�A�#A�^A+A%A��A\)A��AjA�mA�PA7LA��A��AK�A\)AC�A��A�A{Al�A"�A
�A	"�A9XAAĜA��A��A��A��A�A�TA�7AG�A �`A A�@���@��@��;@��h@�  @��\@�r�@�P@�;d@��@��/@�Ĝ@�@�u@旍@��@���@䛦@�(�@㕁@�-@�I�@�V@�l�@�%@�b@׍P@��@�^5@�=q@��@�?}@�j@�t�@ҟ�@с@�/@�bN@ϥ�@�-@�X@��@̼j@�j@���@�hs@���@���@ȼj@�j@�(�@Ƨ�@���@�r�@�  @å�@�K�@�o@��@°!@�@+@�ff@�`B@���@�S�@���@�J@�1@�t�@���@���@���@�n�@�{@��-@��h@�O�@��/@��@��P@�
=@��!@��T@��@� �@�dZ@�;d@�
=@�M�@�J@��-@��j@�  @���@���@���@�%@���@��@�  @���@�|�@�l�@�l�@�|�@�t�@�ȴ@�n�@��+@���@�@���@���@�O�@��j@���@��m@�dZ@��!@�n�@��@��`@��u@�1@��F@�dZ@�@�v�@�-@��@���@�`B@��@��@��@��u@�j@�Z@�9X@� �@�1@��;@���@�\)@�@��\@�J@�`B@�O�@�7L@�V@��/@��9@���@�(�@��P@�33@�"�@��@�^5@��@���@��@�bN@��@���@���@���@�t�@�C�@��y@���@��+@��+@�~�@�V@�$�@��T@�@��^@���@�/@���@�Ĝ@�j@�Q�@�1'@�1@���@��@���@��@���@���@�S�@�@���@��\@�^5@�v�@��@��@�o@�@��H@��+@�n�@�J@��#@���@�@�@��@�?}@��@���@���@�r�@�1'@�1@��;@��w@��F@�t�@�"�@�o@���@��T@�V@��`@��9@�9X@���@���@���@�C�@���@���@��!@���@�~�@�^5@�5?@�$�@�J@��#@��7@�X@�/@��@��@�Ĝ@���@��D@�A�@�ƨ@�K�@�33@�+@���@���@���@�v�@�$�@�J@��#@��^@�`B@���@���@��D@�bN@�A�@�9X@� �@�  @��;@�dZ@�33@�@��@���@�E�@�=q@��@��9@y�#@p��@fȴ@]�h@U`B@O+@HQ�@A�@;@4�@.ff@)�@$��@ȴ@��@p�@-@�+@	7L@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 AݍPAݏ\AݍPAݏ\AݓuAݕ�Aݙ�Aݙ�Aݛ�Aݟ�Aݡ�Aݟ�Aݟ�Aݡ�A݃A�dZA�?}A�;dA�33A�+A��A��A��/Aܺ^A܅A��/AғuAɡ�A�5?A�A�A�33A�r�A��9A��A�$�A��jA�~�A�Q�A���A�-A�r�A�Q�A�  A� �A�A�\)A�I�A��HA��A�33A��A�K�A�~�A�
=A��^A�-A�I�A���A�E�A�9XA��hA�JA�t�A���A���A�n�A���A�M�A���A�=qA��A��jA�33A��\A��#A��A�VA���A���A��A���A�ĜA��A�|�A��uA�hsA��TA��PA�ĜA��DA�$�A���A��wA���A��!A�^5A��uA|�DA{S�Ax��AwoAv�9AtbNAq��AqƨAq
=ApĜApv�Ao�FAn��AlȴAh�/Ae�
A_��A]oA[�AZ{AY�PAYx�AYl�AY/AV9XAP=qAN�AM�AL-AK�-AJ��AG��AG
=AE��AC��AA�FAA+A@ �A<�RA8ffA6�A5t�A4��A3�A2��A21A1"�A0n�A/ƨA.�yA-��A-oA*��A(�A&z�A$��A$�\A#��A"�A"A�A!oA ȴA ��A �A {A��AI�A �A"�AJA�A�#A�^A+A%A��A\)A��AjA�mA�PA7LA��A��AK�A\)AC�A��A�A{Al�A"�A
�A	"�A9XAAĜA��A��A��A��A�A�TA�7AG�A �`A A�@���@��@��;@��h@�  @��\@�r�@�P@�;d@��@��/@�Ĝ@�@�u@旍@��@���@䛦@�(�@㕁@�-@�I�@�V@�l�@�%@�b@׍P@��@�^5@�=q@��@�?}@�j@�t�@ҟ�@с@�/@�bN@ϥ�@�-@�X@��@̼j@�j@���@�hs@���@���@ȼj@�j@�(�@Ƨ�@���@�r�@�  @å�@�K�@�o@��@°!@�@+@�ff@�`B@���@�S�@���@�J@�1@�t�@���@���@���@�n�@�{@��-@��h@�O�@��/@��@��P@�
=@��!@��T@��@� �@�dZ@�;d@�
=@�M�@�J@��-@��j@�  @���@���@���@�%@���@��@�  @���@�|�@�l�@�l�@�|�@�t�@�ȴ@�n�@��+@���@�@���@���@�O�@��j@���@��m@�dZ@��!@�n�@��@��`@��u@�1@��F@�dZ@�@�v�@�-@��@���@�`B@��@��@��@��u@�j@�Z@�9X@� �@�1@��;@���@�\)@�@��\@�J@�`B@�O�@�7L@�V@��/@��9@���@�(�@��P@�33@�"�@��@�^5@��@���@��@�bN@��@���@���@���@�t�@�C�@��y@���@��+@��+@�~�@�V@�$�@��T@�@��^@���@�/@���@�Ĝ@�j@�Q�@�1'@�1@���@��@���@��@���@���@�S�@�@���@��\@�^5@�v�@��@��@�o@�@��H@��+@�n�@�J@��#@���@�@�@��@�?}@��@���@���@�r�@�1'@�1@��;@��w@��F@�t�@�"�@�o@���@��T@�V@��`@��9@�9X@���@���@���@�C�@���@���@��!@���@�~�@�^5@�5?@�$�@�J@��#@��7@�X@�/@��@��@�Ĝ@���@��D@�A�@�ƨ@�K�@�33@�+@���@���@���@�v�@�$�@�J@��#@��^@�`B@���@���@��D@�bN@�A�@�9X@� �@�  @��;@�dZ@�33@�@��@���@�E�@�=qG�O�@��9@y�#@p��@fȴ@]�h@U`B@O+@HQ�@A�@;@4�@.ff@)�@$��@ȴ@��@p�@-@�+@	7L@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B1'B0!B0!B/B/B/B/B-B(�B&�B$�B�B�NB�B`BBP�BL�BI�BN�BL�BI�BC�B?}B=qB:^B33B+B+B,B)�B$�B�BoBVBDB1B%BB  B��B��B�B�B�mB�TB�/B��B��B�wB�RB�-B�'B�B��B��B��B�hB�JB}�BffBL�B@�B9XB/B�B��B�B�3B��B��B�PB{�BffB<jB#�BB
�;B
�^B
�-B
��B
�B
bNB
L�B
>wB
�B
{B
B	��B	�B	�`B	�B	�/B	�5B	�/B	�#B	�
B	��B	ÖB	�B	��B	v�B	e`B	ZB	R�B	O�B	N�B	L�B	H�B	6FB	�B	uB	PB	DB	+B	B��B��B��B�B�B�B�sB�/B�
B�B�B�B�
B��B��B��B��BɺBǮBĜB�}B�RB�-B�B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�JB�7B�%B�B~�B}�B|�B{�By�Bx�Bw�Bv�Bv�Bu�Bt�Bq�Bo�Bl�Bk�BjBiyBiyBhsBgmBe`Be`BdZBbNB`BB`BB_;B^5B^5B^5B]/B]/B\)B\)B[#BZBZBZB[#B[#B[#B\)B\)B[#BZBYB\)B]/B]/B^5B`BB`BBaHBaHB`BBaHBbNBbNBe`BhsBiyBjBjBk�Bk�Bk�Bk�Bl�Bm�Bn�Bp�Bp�Bq�Br�Bt�Bv�Bv�Bv�Bv�By�B|�B� B�B�B�B�B�B�7B�7B�=B�JB�JB�PB�PB�PB�PB�PB�JB�VB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�RB�jB�qB�wBBÖBĜBɺB��B��B�B�#B�BB�HB�ZB�sB�B�B�B��B��B��B	B	%B	DB	PB	\B	hB	hB	uB	�B	�B	�B	!�B	%�B	'�B	,B	-B	.B	/B	1'B	33B	6FB	:^B	<jB	=qB	?}B	A�B	C�B	D�B	H�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	Q�B	S�B	W
B	YB	]/B	cTB	cTB	dZB	gmB	jB	l�B	m�B	o�B	r�B	u�B	u�B	v�B	y�B	|�B	}�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�dB	�}B	ÖB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
VB
	7B
uB
�B
#�B
.B
6FB
8RB
A�B
G�B
M�B
S�B
YB
]/B
`BB
bNB
e`B
iyB
l�B
p�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B1;B19B1;B19B17B19B17B17B17B17B17B19B17B17B03B00B/-B/+B/,B/,B- B)B&�B$�B�B�[B�)B`JBP�BL�BI�BN�BL�BI�BC�B?�B={B:iB3>B+B+B,B*B$�B�BzB^BMB;B/BB 	B��B��B�B�B�xB�`B�9B��B��B�B�_B�4B�0B�B��B��B��B�nB�UB}�BflBL�B@�B9`B/$B�B��B�$B�9B��B��B�UB{�BfnB<sB#�B)B
�GB
�iB
�6B
��B
�%B
bYB
L�B
>�B
�B
�B
$B	��B	��B	�pB	�-B	�BB	�IB	�?B	�4B	�B	��B	èB	�B	��B	v�B	ewB	Z4B	SB	O�B	N�B	L�B	H�B	6]B	�B	�B	iB	[B	FB	&B��B��B��B��B�B�B�B�JB�&B�4B�;B�-B�$B�B�B��B��B��B��BĺB��B�lB�KB�"B�B��B��B��B��B��B��B��B��B��B��B��B�{B�iB�VB�CB�+BB~B}B|By�Bx�Bw�Bv�Bv�Bu�Bt�Bq�Bo�Bl�Bk�Bj�Bi�Bi�Bh�Bg�Be�Be�BdzBbkB`dB`aB_\B^VB^UB^TB]OB]PB\IB\JB[CBZ=BZ>BZ?B[GB[EB[CB\HB\JB[EBZ@BY7B\JB]MB]NB^WB`dB`cBaiBaiB`cBaiBbnBblBe�Bh�Bi�Bj�Bj�Bk�Bk�Bk�Bk�Bl�Bm�Bn�Bp�Bp�Bq�Br�Bt�Bv�Bv�Bv�Bv�By�B}B� B�'B�&B�*B�(B�=B�UB�XB�\B�hB�hB�nB�pB�nB�nB�oB�iB�sB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�'B�7B�PB�oB��B��B��B­BñBĹB��B��B��B�#B�>B�^B�bB�uB�B�B��B��B��B�B�B	#B	>B	ZB	jB	tB	�B	B	�B	�B	�B	�B	!�B	%�B	(B	,B	-%B	.-B	/3B	1?B	3IB	6]B	:wB	<�B	=�B	?�B	A�B	C�B	D�B	H�B	I�B	K�B	L�B	M�B	N�B	O�B	P�B	RB	TB	WB	Y.B	]BB	ckB	ckB	dpB	g�B	j�B	l�B	m�B	o�B	r�B	u�B	u�B	v�B	y�B	}B	~B	�B	�9B	�LB	�QB	�YB	�\B	�dB	�iB	�kB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�,B	�>B	�EB	�QB	�yB	��B	èB	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�"B	�'B	�.B	�/B	�4B	�=B	�AB	�IB	�GB	�QB	�YB	�fB	�eB	�fB	�jB	�kB	�jB	�fB	�fB	�kB	�sB	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	� B	�
B	�B	�
B	�B
 B
 B
 B
 B
 B
B
B
B
#B
&B
*B
0G�O�B
	JB
�B
�B
#�B
.%B
6WB
8dB
A�B
G�B
M�B
TB
Y$B
]<B
`NB
b_B
eoB
i�B
l�B
p�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.08 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230533  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230533  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                