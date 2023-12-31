CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-22T06:53:19Z creation;2023-05-22T06:53:20Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230522065319  20230522070345  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�-m˩�1   @�-	P��&@;Q���l��c�G�z�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  @���A   A@  A`  A�  A�33A�  A�  A�33A�33A�  A�33B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B���B�  B�  C   C  C  C�fC�fC
  C  C�C  C�fC  C  C  C�fC�fC  C �C"�C$  C&  C(�C*�C,  C.  C/�fC2  C4  C6�C8  C:  C;�fC>  C@  CB  CC�fCF  CH  CJ  CK�fCN  CP  CR  CS�fCV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C��C��3C�  C�  C�  C��C��3C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C��3C��C��C�  C�  C��C��C��C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C��3C��3C�  C�  C��C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+�fD,fD,�fD-fD-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<y�D<��D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH�fDI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP�fDQ  DQ� DR  DR� DS  DS� DTfDT�fDU  DU� DV  DV� DWfDW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�|�D���D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�<�D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D3D��3D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̓3D�� D�3D�C3D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�Dܼ�D�  D�C3D݃3D��3D�  D�@ Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�C3D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@��A (�A@(�A`(�A�{A�G�A�{A�{A�G�A�G�A�{A�G�B 
=B
=B
=B
=B 
=B(
=B0
=B8p�B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B���B�B�B�B�B���B���B�B�C �C�C�C��C��C
�C�C)C�C��C�C�C�C��C��C�C )C")C$�C&�C()C*)C,�C.�C/��C2�C4�C6)C8�C:�C;��C>�C@�CB�CC��CF�CH�CJ�CK��CN�CP�CR�CS��CV�CX�CY��C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C}��C�HC�C��{C�HC�HC�HC�C��{C�HC�HC�HC�C�HC�HC�C�C�HC�HC�HC��{C�C�C�HC�HC�C�C�C�HC�C�HC�HC�HC�HC��{C�HC�HC��{C�HC�HC��{C��{C��{C��{C�HC�HC�C�C�HC�HC��{C��{C��{C�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�C�C�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�C�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC��{D ��D �Dz>D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D)�>D*��D+ �D+�
D,
D,�
D-
D-��D. �D.z>D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9
D9��D: �D:��D; �D;��D< �D<z>D<�>D=��D=�>D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH
DH�
DI �DI��DJ �DJ��DK �DKz>DL �DL��DM �DM��DN �DN��DO �DOz>DP �DP�
DQ �DQ��DR �DR��DS �DS��DT
DT�
DU �DU��DV �DV��DW
DW��DX �DX�
DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De�
Df �Df��Dg �Dg��Dh �Dhz>Dh�>Di��Dj �Dj�
Dk
Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp
Dp��Dq �Dq��Dq�>Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D�}D��RD��D�@RD��RD��RD� RD�C�D��RD��RD��D�@RD�}D��D��D�=D�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD��D�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�=D��RD��RD��D�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��D� RD�C�D��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�C�D��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�D�ÅD��D�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�C�D̓�D��RD��D�C�D΀RDνD� RD�@RDπRD��RD� RD�@RDЀRDнD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRDֽD� RD�C�D׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD�}DܽD� RD�C�D݃�D�ÅD� RD�@RDރ�D��RD� RD�@RD߀RD��RD� RD�@RD��RD�D��D�@RD�RD��RD� RD�@RD�RD��RD� RD�@RDヅD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�C�D�RD�D� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��GA��pA���A�IA��A� iA��vA���A�m�A��A��gA��+A�|�A�E�A��A��`A���A��BA��9A���A�ƨA��HA���A��A���A��VA��uA���A�kA�:�A��A��5A�3�A��A�v`A��A�C-A�֡A��GA���A��A��6A�A�*eA���A�,�A�	�A��vA�ܒA���A�E�A���A�m�A��jA�B'A�j�A���A�xlA�lWA�v�A��A�I�A��A�[�A��)A�6�A���A�D3A� iA���A���A�m]A�&A��A���A�'�A���A�ΥA���A���A���A�EmA�u%A���A���A��A�_A�DA��A��A���A��DA�!�A�|�A�~A�]dA�U�A���A��A��tA���A�*eA��]A�"A���A�e�A��A��A�RTA��A}�)A|#�Az�]AzOAxzAs�Aq�vApcAn��Am�Ak�[Ai�)Ag��Ae~�Ad!-Ab�&Ab/�Aa�3AaA�A`�A_��A]33A\DgA[�A[P�AZ��AZ��AZB[AY�AX\�AV:�AS�~AP�{AN�AAM�AL�AJ?}AI��AH��AH�*AH�AG��AGC�AF��AF�XAEɆAD�AD_�AD7�AC�AB	lAA��A@F�A?��A?$�A>�8A>�A>��A>T�A=l"A;�A:� A9�=A7�"A6l"A5��A4��A3tTA2�IA1��A0A A.��A-��A,��A+y�A*��A)��A)?�A)�A(zA(�A'%�A%�`A%�A$OA#�]A#�A#rGA"��A!�hA x�Av`AoiA��A� AjAY�AOA3�A_A�9AtTAU2A#�A�zA*�A�7A�9AP�A�jA�'A-wA�A��A#�A�A^5A'�A_�Av�A��A�A��A�A�|A��A
��A	��A�A$tA�A�AqA͟A�A�MA��AA a|@��@�U�@��@�֡@���@�5?@�g�@��@�_@��O@��@�l�@��_@�@��@�=�@�x�@���@�$�@��@��@�~@椩@�Y@��@�@O@�'R@�>�@�m�@�E9@��p@�9�@י�@���@ј�@�3�@��K@�K^@���@�_p@��[@��@���@ǧ�@�-�@��P@�D�@��@���@��@��@�B[@��0@�^�@�C@��@�� @�p;@�?}@��8@�kQ@���@���@�A @�}V@�+@��f@��B@��x@�0U@���@���@��z@��j@���@���@���@�e@���@�%@��z@�D�@�<6@���@�خ@��@���@��O@�&�@���@���@�Y�@��@�`�@���@��@��[@��6@��@���@�@O@��@��L@�E�@���@�iD@��]@�!�@��@�q@�^5@��T@���@�o�@�A @�u�@�*�@���@��7@�֡@�R�@�@��w@��@�v`@�6z@�ی@���@�j@���@��@�w�@�;�@�!@�4@��@���@�|�@��y@���@�y>@�_@�M@�C�@�@�@�/�@��.@��#@��P@�c@�w2@�Y�@�<6@�+@�kQ@��@���@���@���@�@�|�@�$@���@�#�@��,@�	@�ԕ@���@�T�@�@O@��_@�g8@�Z�@�7�@�$@��@�M@�ݘ@���@��F@��a@��[@��@�.I@�ȴ@��O@���@��}@��I@�#:@��[@���@���@�4@���@�|�@�^5@�B[@��9@�j�@�g�@�`B@�U�@�Vm@�S�@�G�@�33@�'�@��@���@��K@���@���@��D@�j@�GE@� �@�m@��@~�@~��@}��@}#�@|�E@|z�@|6@{�@{�@{��@{�	@z��@ys�@x�/@w�m@w$t@v?@u�d@uO�@u�@t�f@tی@t��@t-�@s�0@sS�@sC@r��@r}V@rn�@rc @rM�@r8�@r�@r�@r	@r	@q��@q�N@q�^@q�-@q@o|�@o&@n�h@nYK@n	@m�Z@m�@m��@m�@m%@l��@l��@l�@k��@k��@kj�@ko@j�@j6�@i4@h�I@g�A@g�@e��@d(�@cZ�@c�@c�@b��@b�c@b�@b�H@b�]@b�}@b.�@a�@a��@aIR@a8�@a+�@a�@a�@`�@`�@`e�@_��@_(@^�'@^q�@^J@]�@\��@[خ@[��@[�V@[Mj@Z�@Y��@Y�j@Y��@Y��@Y|@Y=�@Y�@X�9@Xu�@X-�@W�
@Wt�@V�s@V҉@V�s@Vz@Vi�@VB[@U��@U��@T��@S�K@S{J@Ss@SP�@SC�@S$t@R�8@R҉@R��@R� @RM�@Q��@P�@P:�@O�@OH�@O'�@N�"@N�B@N��@Nu%@NM�@N($@N	@M�7@L֡@K|�@K�@J��@J�]@J�,@J��@J�@J��@Jz@JkQ@J@�@J8�@J.�@JO@I�.@I��@Hl"@HV�@HD�@H7@G�@GW?@G9�@F��@F��@F$�@E`B@EV@D�.@D1'@D1@C��@Cv`@C�@B�r@B	@Ac@A@@�`@@�j@@oi@@�@?�[@?x@?"�@>��@>�@>�L@>kQ@>R�@>;�@>)�@>�@=��@=�@=�X@=�S@=s�@=Y�@<�p@;��@;��@;K�@;&@;@:�"@:�@:��@9��@9��@9�~@9s�@9T�@9�@8�	@8�D@84n@8�@7�	@6�2@6=q@4Ɇ@4�@4l"@4�@3x@3U�@3A�@3�@2�@2�X@2�@2�b@2� @2}V@2u%@2c @2V@2Ov@2@�@2�@1�d@1��@1��@1�@1zx@1j@1`B@1J�@18�@0S�@/K�@.�c@.�H@.��@.�@.l�@.YK@.Q@.0U@-�.@-��@-�j@-��@-��@-��@-w2@-Y�@-0�@,��@+��@+�@+$t@+!-@+'�@+�@+�@*�@*B[@)��@)��@)2a@)@(��@(�U@(��@(u�@(1'@'�r@'��@'ݘ@'��@'�}@'��@'��@'��@'iD@']�@'H�@'+@'�@&��@&n�@&V@&.�@%�>@%�-@%=�@$�D@$I�@$:�@$%�@#خ@#1�@"��@"��@"�@"��@"�@"��@"��@"��@"h
@!�D@!�h@!w2@!rG@!7L@ �@ �e@ �Y@ _@��@�,@��@W�@�@��@�'@��@#�@�@|�@I�@	�@�Q@�Q@��@�w@o�@a@E9@&@o@��@��@s�@=q@!�@@�@ԕ@�n@&�@��@��@�5@�K@Ɇ@�@~(@w�@Q�@�@��@��@~�@/�@�@��@�1@h
@3�@�@j@�@�v@��@�@tT@�@��@��@�f@E9@�@��@��@xl@v�@kQ@e@��@�d@o @G�@+�@�@�@%@�/@��@Ĝ@�U@�$@�9@��@�@�D@C-@*�@-�@(�@x@�k@+@ i@�B@��@�x@u%@@�@5?@.�@($@��@�@�e@��@z�@l"@j@U2@M@��@خ@˒@�@�	@s@Y@
}V@
^5@
E�@
)�@
�@	�D@	�@	��@	�T@	ԕ@	��@	��@	e,@	<6@	-w@	!�@	�@	�@Ĝ@oi@N�@:�@1'@(�@!@x@�g@�@�:@A�@@�H@�@�,@�@��@;�@�@��@�@�~@�@w2@L�@�@;@�|@�K@�`@��@֡@�_@��@��@�o@�o@�o@h�@I�@�r@b�@"�@�@��@��@L0@	@�@k�@�@@ �@ ��@ u�@ u�@ :�@ �?��$?�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��GA��pA���A�IA��A� iA��vA���A�m�A��A��gA��+A�|�A�E�A��A��`A���A��BA��9A���A�ƨA��HA���A��A���A��VA��uA���A�kA�:�A��A��5A�3�A��A�v`A��A�C-A�֡A��GA���A��A��6A�A�*eA���A�,�A�	�A��vA�ܒA���A�E�A���A�m�A��jA�B'A�j�A���A�xlA�lWA�v�A��A�I�A��A�[�A��)A�6�A���A�D3A� iA���A���A�m]A�&A��A���A�'�A���A�ΥA���A���A���A�EmA�u%A���A���A��A�_A�DA��A��A���A��DA�!�A�|�A�~A�]dA�U�A���A��A��tA���A�*eA��]A�"A���A�e�A��A��A�RTA��A}�)A|#�Az�]AzOAxzAs�Aq�vApcAn��Am�Ak�[Ai�)Ag��Ae~�Ad!-Ab�&Ab/�Aa�3AaA�A`�A_��A]33A\DgA[�A[P�AZ��AZ��AZB[AY�AX\�AV:�AS�~AP�{AN�AAM�AL�AJ?}AI��AH��AH�*AH�AG��AGC�AF��AF�XAEɆAD�AD_�AD7�AC�AB	lAA��A@F�A?��A?$�A>�8A>�A>��A>T�A=l"A;�A:� A9�=A7�"A6l"A5��A4��A3tTA2�IA1��A0A A.��A-��A,��A+y�A*��A)��A)?�A)�A(zA(�A'%�A%�`A%�A$OA#�]A#�A#rGA"��A!�hA x�Av`AoiA��A� AjAY�AOA3�A_A�9AtTAU2A#�A�zA*�A�7A�9AP�A�jA�'A-wA�A��A#�A�A^5A'�A_�Av�A��A�A��A�A�|A��A
��A	��A�A$tA�A�AqA͟A�A�MA��AA a|@��@�U�@��@�֡@���@�5?@�g�@��@�_@��O@��@�l�@��_@�@��@�=�@�x�@���@�$�@��@��@�~@椩@�Y@��@�@O@�'R@�>�@�m�@�E9@��p@�9�@י�@���@ј�@�3�@��K@�K^@���@�_p@��[@��@���@ǧ�@�-�@��P@�D�@��@���@��@��@�B[@��0@�^�@�C@��@�� @�p;@�?}@��8@�kQ@���@���@�A @�}V@�+@��f@��B@��x@�0U@���@���@��z@��j@���@���@���@�e@���@�%@��z@�D�@�<6@���@�خ@��@���@��O@�&�@���@���@�Y�@��@�`�@���@��@��[@��6@��@���@�@O@��@��L@�E�@���@�iD@��]@�!�@��@�q@�^5@��T@���@�o�@�A @�u�@�*�@���@��7@�֡@�R�@�@��w@��@�v`@�6z@�ی@���@�j@���@��@�w�@�;�@�!@�4@��@���@�|�@��y@���@�y>@�_@�M@�C�@�@�@�/�@��.@��#@��P@�c@�w2@�Y�@�<6@�+@�kQ@��@���@���@���@�@�|�@�$@���@�#�@��,@�	@�ԕ@���@�T�@�@O@��_@�g8@�Z�@�7�@�$@��@�M@�ݘ@���@��F@��a@��[@��@�.I@�ȴ@��O@���@��}@��I@�#:@��[@���@���@�4@���@�|�@�^5@�B[@��9@�j�@�g�@�`B@�U�@�Vm@�S�@�G�@�33@�'�@��@���@��K@���@���@��D@�j@�GE@� �@�m@��@~�@~��@}��@}#�@|�E@|z�@|6@{�@{�@{��@{�	@z��@ys�@x�/@w�m@w$t@v?@u�d@uO�@u�@t�f@tی@t��@t-�@s�0@sS�@sC@r��@r}V@rn�@rc @rM�@r8�@r�@r�@r	@r	@q��@q�N@q�^@q�-@q@o|�@o&@n�h@nYK@n	@m�Z@m�@m��@m�@m%@l��@l��@l�@k��@k��@kj�@ko@j�@j6�@i4@h�I@g�A@g�@e��@d(�@cZ�@c�@c�@b��@b�c@b�@b�H@b�]@b�}@b.�@a�@a��@aIR@a8�@a+�@a�@a�@`�@`�@`e�@_��@_(@^�'@^q�@^J@]�@\��@[خ@[��@[�V@[Mj@Z�@Y��@Y�j@Y��@Y��@Y|@Y=�@Y�@X�9@Xu�@X-�@W�
@Wt�@V�s@V҉@V�s@Vz@Vi�@VB[@U��@U��@T��@S�K@S{J@Ss@SP�@SC�@S$t@R�8@R҉@R��@R� @RM�@Q��@P�@P:�@O�@OH�@O'�@N�"@N�B@N��@Nu%@NM�@N($@N	@M�7@L֡@K|�@K�@J��@J�]@J�,@J��@J�@J��@Jz@JkQ@J@�@J8�@J.�@JO@I�.@I��@Hl"@HV�@HD�@H7@G�@GW?@G9�@F��@F��@F$�@E`B@EV@D�.@D1'@D1@C��@Cv`@C�@B�r@B	@Ac@A@@�`@@�j@@oi@@�@?�[@?x@?"�@>��@>�@>�L@>kQ@>R�@>;�@>)�@>�@=��@=�@=�X@=�S@=s�@=Y�@<�p@;��@;��@;K�@;&@;@:�"@:�@:��@9��@9��@9�~@9s�@9T�@9�@8�	@8�D@84n@8�@7�	@6�2@6=q@4Ɇ@4�@4l"@4�@3x@3U�@3A�@3�@2�@2�X@2�@2�b@2� @2}V@2u%@2c @2V@2Ov@2@�@2�@1�d@1��@1��@1�@1zx@1j@1`B@1J�@18�@0S�@/K�@.�c@.�H@.��@.�@.l�@.YK@.Q@.0U@-�.@-��@-�j@-��@-��@-��@-w2@-Y�@-0�@,��@+��@+�@+$t@+!-@+'�@+�@+�@*�@*B[@)��@)��@)2a@)@(��@(�U@(��@(u�@(1'@'�r@'��@'ݘ@'��@'�}@'��@'��@'��@'iD@']�@'H�@'+@'�@&��@&n�@&V@&.�@%�>@%�-@%=�@$�D@$I�@$:�@$%�@#خ@#1�@"��@"��@"�@"��@"�@"��@"��@"��@"h
@!�D@!�h@!w2@!rG@!7L@ �@ �e@ �Y@ _@��@�,@��@W�@�@��@�'@��@#�@�@|�@I�@	�@�Q@�Q@��@�w@o�@a@E9@&@o@��@��@s�@=q@!�@@�@ԕ@�n@&�@��@��@�5@�K@Ɇ@�@~(@w�@Q�@�@��@��@~�@/�@�@��@�1@h
@3�@�@j@�@�v@��@�@tT@�@��@��@�f@E9@�@��@��@xl@v�@kQ@e@��@�d@o @G�@+�@�@�@%@�/@��@Ĝ@�U@�$@�9@��@�@�D@C-@*�@-�@(�@x@�k@+@ i@�B@��@�x@u%@@�@5?@.�@($@��@�@�e@��@z�@l"@j@U2@M@��@خ@˒@�@�	@s@Y@
}V@
^5@
E�@
)�@
�@	�D@	�@	��@	�T@	ԕ@	��@	��@	e,@	<6@	-w@	!�@	�@	�@Ĝ@oi@N�@:�@1'@(�@!@x@�g@�@�:@A�@@�H@�@�,@�@��@;�@�@��@�@�~@�@w2@L�@�@;@�|@�K@�`@��@֡@�_@��@��@�o@�o@�o@h�@I�@�r@b�@"�@�@��@��@L0@	@�@k�@�@@ �@ ��@ u�@ u�@ :�@ �?��$?�8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�RB��B��B�0B��B�YB��B�TB��B�B�sB�sB�8B�B��B�B�FB��B�`B��B��B��B��B��B�,B�,B��B��B�:B�-B�VB��B��B�B��B�B�]B��B��B�B��B�>B�TB�B�B�rB��B�dB��B�)B�lB��B}"BzxBt�Bb�BFB5�B0�B:xBF�BDgB6+B%FB# B(>B+6B#�B�B/B_BB2BvB6BB��B�B� B�By>Bc�BW$BGEB<B6B(�B
�B�(B��B��B�	B�B�QB�\B�MB�RB��B��B��B��ByrBh�Bb�BV�B>]B-�B�BDB
��B
��B
�]B
��B
�QB
��B
��B
�?B
��B
�B
�sB
�"B
�_B
��B
x�B
poB
l�B
f�B
b�B
a�B
^5B
\CB
W$B
J=B
BuB
>wB
;0B
9$B
6FB
4�B
0�B
)yB
WB
SB	��B	�B	��B	�/B	��B	�MB	�B	ҽB	ЗB	ΥB	̘B	�DB	��B	�?B	��B	� B	��B	�dB	�?B	�B	� B	�KB	�B	�
B	��B	��B	��B	�bB	��B	��B	�B	��B	�gB	��B	~B	x�B	v`B	s�B	m�B	i�B	e�B	cTB	`�B	]IB	Y�B	V�B	T�B	R�B	O�B	L�B	G�B	C{B	@B	>�B	<�B	:�B	8�B	4�B	.B	)�B	&2B	"4B	�B	IB	�B	�B	�B	qB	WB	B	�B	�B	�B	�B	B	bB	�B	�B	
�B	�B	�B	�B	�B	oB�B��B��B�8B��B��B�B��B��B�QB��B�B�`B��B�B��B�!B�	B��BخB�
B�gB�FB��B�B�B�B��B��BөB҉B҉B� BϑB��B�pB�~B�=B�#B��B��B�9B�MBB�'B��B��B��B��B��B�BB��B��B�B��B��B�B��B��B�B��B�nB�nB�B��B�GB��B��B��B��B�FB��B��B�DB��B�B��B�B�PB�B�B��B�uB�'B��B�+B��B�#B�B�0B�JB�~B�6B��B��BѝB��B��B��B�OBޞB�\B�B��B�NB�zB�fB�B�B�oB�B��B�2B�B��B��B�qB	 �B	�B	B	�B	fB		�B	�B	�B	�B	BB	�B	�B	2B	�B	�B	=B	�B	�B	B	VB	 vB	&�B	(�B	*B	-B	1vB	4�B	7fB	;�B	=�B	B[B	C�B	FYB	G�B	IlB	OB	T�B	X�B	ZQB	[	B	[�B	\�B	]�B	`'B	ezB	h�B	i*B	jB	j�B	kB	k6B	k�B	m�B	o5B	s3B	tB	tTB	u�B	v�B	xB	~�B	��B	��B	�[B	�MB	�dB	�B	�
B	��B	��B	�bB	��B	�6B	��B	��B	�B	�nB	��B	��B	�^B	��B	�dB	�0B	�4B	�lB	�#B	�JB	�bB	�B	��B	�B	�B	�!B	�B	߾B	�B	�XB	��B	�*B	�CB	�GB	��B	��B	��B	��B	�B	�B	�jB	��B	��B	�B	��B	�BB	��B	��B
 iB
 �B
[B
GB
�B
�B
�B
+B
�B
	�B
�B
�B
.B
�B
�B
SB
�B
�B
eB
B
�B
�B
!�B
#�B
'�B
*�B
.B
/�B
1�B
2�B
33B
3�B
4�B
6`B
8�B
:^B
;0B
<�B
>B
>wB
>�B
?cB
@ B
@�B
@�B
AB
A;B
AUB
BB
B'B
A�B
DMB
I�B
J�B
L�B
M�B
N�B
O(B
O�B
O�B
P�B
R�B
TFB
T{B
T�B
W�B
X�B
ZB
[�B
]�B
^�B
cB
d�B
f2B
gRB
i�B
o�B
r�B
tnB
tnB
t�B
uB
t�B
uZB
u?B
u�B
w�B
x�B
y�B
zDB
zxB
z�B
z�B
{0B
{dB
{�B
|�B
}�B
�4B
��B
�oB
�[B
�-B
��B
�lB
��B
��B
�B
��B
��B
��B
�HB
��B
�NB
��B
�TB
�[B
��B
�{B
�gB
�mB
�yB
�+B
�EB
�KB
�eB
��B
��B
�WB
�IB
��B
�|B
��B
��B
�B
�hB
��B
�:B
�nB
��B
��B
�B
��B
��B
��B
�wB
��B
�IB
��B
�OB
��B
�B
�UB
��B
�GB
�9B
�$B
�B
��B
��B
�B
�JB
��B
�B
�B
�B
�6B
�PB
�6B
�PB
�6B
��B
�.B
�B
�HB
��B
��B
�UB
�oB
�B
�uB
�{B
ŢB
��B
�+B
��B
�1B
��B
�lB
��B
�DB
�B
�B
��B
�"B
�pB
�B
ϫB
�bB
��B
��B
҉B
�@B
�uB
�B
�,B
�,B
�aB
�{B
��B
�B
�gB
ՁB
՛B
՛B
ּB
��B
�_B
��B
�1B
�KB
�B
��B
�7B
�=B
ۦB
�B
�]B
�xB
��B
��B
��B
ބB
ބB
߾B
�vB
��B
�tB
�ZB
��B
�zB
��B
��B
�B
�RB
��B
��B
�>B
�$B
�XB
�B
�B
��B
��B
��B
��B
�yB
��B
��B
�B
�KB
�B
�B
�B
�B
�eB
�B
�B
�B
�B
� B
�iB
�B
�B
�B
�B
�;B
�UB
��B
�B
��B
�B
�AB
�[B
�B
�|B
�B
�%B
�B
�%B
�%B
�%B
��B
��B
�`B
��B
�fB
�B
�lB
��B
��B
�$B
�XB
��B
�DB
�DB
�xB
��B
��B
��B
��B
��B
�JB
�JB
�B
�B
��B
��B
��B
�jB
��B
�"B
�VB
�]B
�cB
��B
��B
��B iB�BB�B�BB�BAB[B�B�B{BB3B3B�BB9BSBSBYB�B�BKB�B	B	B	B	�B
	B
�B
�B)BxB^BDB�B0BB0BdB~B�BB�B�BBB"B<B�BB.B.BHBHB�B�B�B4B�B�B�BhB�B:B�B�B�B@BuB�B�BBgBgB�B�B$B�B_B�BKB�BBkB�B�B�B	B#BWBBBCBxB]BxB�B�B�B�B�B�B�B�B�BdB�B�B�B�B�BB;B�B�B�B�B BB BB 'B B �B!�B!�B!�B!�B!�B!�B!�B"hB"�B"�B"�B"�B#:B# B#�B$�B$�B$�B$�B$�B$�B%,B%B%,B%FB%FB%�B%�B%�B%�B%�B%�B%�B&�B'B'B'8B'RB'mB'mB'�B'�B'�B($B(�B(�B)*B)B)*B)_B)�B*0B*�B+B+QB+QB+kB+�B+�B,qB,�B,�B,�B,�B,�B,�B-B-B-)B-]B-]B-wB-�B-�B./B.�B.�B.�B.�B.�B/5B/�B/�B0�B0�B0�B1vB1�B2-B2-B2aB2�B2�B3444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�RB��B��B�0B��B�YB��B�TB��B�B�sB�sB�8B�B��B�B�FB��B�`B��B��B��B��B��B�,B�,B��B��B�:B�-B�VB��B��B�B��B�B�]B��B��B�B��B�>B�TB�B�B�rB��B�dB��B�)B�lB��B}"BzxBt�Bb�BFB5�B0�B:xBF�BDgB6+B%FB# B(>B+6B#�B�B/B_BB2BvB6BB��B�B� B�By>Bc�BW$BGEB<B6B(�B
�B�(B��B��B�	B�B�QB�\B�MB�RB��B��B��B��ByrBh�Bb�BV�B>]B-�B�BDB
��B
��B
�]B
��B
�QB
��B
��B
�?B
��B
�B
�sB
�"B
�_B
��B
x�B
poB
l�B
f�B
b�B
a�B
^5B
\CB
W$B
J=B
BuB
>wB
;0B
9$B
6FB
4�B
0�B
)yB
WB
SB	��B	�B	��B	�/B	��B	�MB	�B	ҽB	ЗB	ΥB	̘B	�DB	��B	�?B	��B	� B	��B	�dB	�?B	�B	� B	�KB	�B	�
B	��B	��B	��B	�bB	��B	��B	�B	��B	�gB	��B	~B	x�B	v`B	s�B	m�B	i�B	e�B	cTB	`�B	]IB	Y�B	V�B	T�B	R�B	O�B	L�B	G�B	C{B	@B	>�B	<�B	:�B	8�B	4�B	.B	)�B	&2B	"4B	�B	IB	�B	�B	�B	qB	WB	B	�B	�B	�B	�B	B	bB	�B	�B	
�B	�B	�B	�B	�B	oB�B��B��B�8B��B��B�B��B��B�QB��B�B�`B��B�B��B�!B�	B��BخB�
B�gB�FB��B�B�B�B��B��BөB҉B҉B� BϑB��B�pB�~B�=B�#B��B��B�9B�MBB�'B��B��B��B��B��B�BB��B��B�B��B��B�B��B��B�B��B�nB�nB�B��B�GB��B��B��B��B�FB��B��B�DB��B�B��B�B�PB�B�B��B�uB�'B��B�+B��B�#B�B�0B�JB�~B�6B��B��BѝB��B��B��B�OBޞB�\B�B��B�NB�zB�fB�B�B�oB�B��B�2B�B��B��B�qB	 �B	�B	B	�B	fB		�B	�B	�B	�B	BB	�B	�B	2B	�B	�B	=B	�B	�B	B	VB	 vB	&�B	(�B	*B	-B	1vB	4�B	7fB	;�B	=�B	B[B	C�B	FYB	G�B	IlB	OB	T�B	X�B	ZQB	[	B	[�B	\�B	]�B	`'B	ezB	h�B	i*B	jB	j�B	kB	k6B	k�B	m�B	o5B	s3B	tB	tTB	u�B	v�B	xB	~�B	��B	��B	�[B	�MB	�dB	�B	�
B	��B	��B	�bB	��B	�6B	��B	��B	�B	�nB	��B	��B	�^B	��B	�dB	�0B	�4B	�lB	�#B	�JB	�bB	�B	��B	�B	�B	�!B	�B	߾B	�B	�XB	��B	�*B	�CB	�GB	��B	��B	��B	��B	�B	�B	�jB	��B	��B	�B	��B	�BB	��B	��B
 iB
 �B
[B
GB
�B
�B
�B
+B
�B
	�B
�B
�B
.B
�B
�B
SB
�B
�B
eB
B
�B
�B
!�B
#�B
'�B
*�B
.B
/�B
1�B
2�B
33B
3�B
4�B
6`B
8�B
:^B
;0B
<�B
>B
>wB
>�B
?cB
@ B
@�B
@�B
AB
A;B
AUB
BB
B'B
A�B
DMB
I�B
J�B
L�B
M�B
N�B
O(B
O�B
O�B
P�B
R�B
TFB
T{B
T�B
W�B
X�B
ZB
[�B
]�B
^�B
cB
d�B
f2B
gRB
i�B
o�B
r�B
tnB
tnB
t�B
uB
t�B
uZB
u?B
u�B
w�B
x�B
y�B
zDB
zxB
z�B
z�B
{0B
{dB
{�B
|�B
}�B
�4B
��B
�oB
�[B
�-B
��B
�lB
��B
��B
�B
��B
��B
��B
�HB
��B
�NB
��B
�TB
�[B
��B
�{B
�gB
�mB
�yB
�+B
�EB
�KB
�eB
��B
��B
�WB
�IB
��B
�|B
��B
��B
�B
�hB
��B
�:B
�nB
��B
��B
�B
��B
��B
��B
�wB
��B
�IB
��B
�OB
��B
�B
�UB
��B
�GB
�9B
�$B
�B
��B
��B
�B
�JB
��B
�B
�B
�B
�6B
�PB
�6B
�PB
�6B
��B
�.B
�B
�HB
��B
��B
�UB
�oB
�B
�uB
�{B
ŢB
��B
�+B
��B
�1B
��B
�lB
��B
�DB
�B
�B
��B
�"B
�pB
�B
ϫB
�bB
��B
��B
҉B
�@B
�uB
�B
�,B
�,B
�aB
�{B
��B
�B
�gB
ՁB
՛B
՛B
ּB
��B
�_B
��B
�1B
�KB
�B
��B
�7B
�=B
ۦB
�B
�]B
�xB
��B
��B
��B
ބB
ބB
߾B
�vB
��B
�tB
�ZB
��B
�zB
��B
��B
�B
�RB
��B
��B
�>B
�$B
�XB
�B
�B
��B
��B
��B
��B
�yB
��B
��B
�B
�KB
�B
�B
�B
�B
�eB
�B
�B
�B
�B
� B
�iB
�B
�B
�B
�B
�;B
�UB
��B
�B
��B
�B
�AB
�[B
�B
�|B
�B
�%B
�B
�%B
�%B
�%B
��B
��B
�`B
��B
�fB
�B
�lB
��B
��B
�$B
�XB
��B
�DB
�DB
�xB
��B
��B
��B
��B
��B
�JB
�JB
�B
�B
��B
��B
��B
�jB
��B
�"B
�VB
�]B
�cB
��B
��B
��B iB�BB�B�BB�BAB[B�B�B{BB3B3B�BB9BSBSBYB�B�BKB�B	B	B	B	�B
	B
�B
�B)BxB^BDB�B0BB0BdB~B�BB�B�BBB"B<B�BB.B.BHBHB�B�B�B4B�B�B�BhB�B:B�B�B�B@BuB�B�BBgBgB�B�B$B�B_B�BKB�BBkB�B�B�B	B#BWBBBCBxB]BxB�B�B�B�B�B�B�B�B�BdB�B�B�B�B�BB;B�B�B�B�B BB BB 'B B �B!�B!�B!�B!�B!�B!�B!�B"hB"�B"�B"�B"�B#:B# B#�B$�B$�B$�B$�B$�B$�B%,B%B%,B%FB%FB%�B%�B%�B%�B%�B%�B%�B&�B'B'B'8B'RB'mB'mB'�B'�B'�B($B(�B(�B)*B)B)*B)_B)�B*0B*�B+B+QB+QB+kB+�B+�B,qB,�B,�B,�B,�B,�B,�B-B-B-)B-]B-]B-wB-�B-�B./B.�B.�B.�B.�B.�B/5B/�B/�B0�B0�B0�B1vB1�B2-B2-B2aB2�B2�B3444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230522065317  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230522065319  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230522065319  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230522065320                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230522065320  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230522065320  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230522070345                      G�O�G�O�G�O�                