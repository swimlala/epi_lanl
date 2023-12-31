CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:32Z creation;2019-05-23T10:00:33Z conversion to V3.1;2022-08-02T05:12:39Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100032  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               
A   JA  A30_8420_010                    2C  D   APEX                            8420                            2.11.2                          846 @ؽ<�R�1   @ؽ<�'q�@+�)^�	�d��䎊1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33Bי�B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C�fC  C�fC  C
  C  C  C  C  C  C  C  C33C�3C�fC   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CI��CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @:=q@�G�@��
A�A!�A@��A`Q�A�=qA�ffA���A�z�A��\AЏ\A�(�A�z�B \)BQ�B\)B=qB 33B(G�B0Q�B8ffB@Q�BHQ�BP=qBXG�B`��BhG�Bp=qBx33B�\B�\B�{B�{B��B�\B�(�B�.B���B�  B�B�B�\B��B�#�B�#�B�#�B��B�#�B�33B��HB�L�B���B��B�\B�\B�{B�\B��B�#�B�(�B�33C B�C  C�C��CC

=C\C�C�C\C
=C�C\C=qCC��C \C"{C$
C&\C(�C*�C,�C.�C0C2�C4\C6\C8{C:{C<�C>�C@CB�CD.CF.CH{CI�HCLCN\CP\CR�CT�CV�CX\CZ�C\�C^�C`
Cb{Cd{Cf{Ch�Cj
=Cl�Cn�Cp�Cr\Ct\Cv{Cx�Cz�C|�C~
C��C��C��C�C�fC�
=C�
=C��C��C�C��C��C��C��C�fC�C�C�fC��C��C��C��C��C��C�
=C�
=C�fC��C�C��C��C��C��C��C�fC��C��C��C�fC�C��C��C�fC��C��C�fC�fC�fC��C�
=C��C��C�fC��C��C��C�
=C�fC��C�fC��C��C��C��C�
=C��C��C��C�fC�
=C��C��C�fC�
=C�
=C��C��C��C��C��C��C�
=C�
=C��C��C�C��C��C�fC��C�fC�
=C��C��C��C�fC��C�
=C�
=C��C�
=C��C�C�fC��C�fC��C�
=C�fC��C��C�fC��C��C��C��C�C��C�
=C��C��C��C�
=C��C�
=C��C�
=C�fD �D ��DD�D{D�{D�D�{D3D��DD�DD�{D�D��D{D�3D	�D	�3D
�D
��D{D�{D{D�DD�{D{D�3D�D��D{D��D�D�HD�D�3D3D��D�D�3D�D��D{D�fD3D��D�D��D�D�3D{D�fD�D�D{D��D3D��D�D�3D{D��D 3D �D!D!��D"�D"��D#{D#�D$�D$�3D%D%�3D&�D&��D'�D'��D(fD(��D)�D)��D*{D*�D+fD+�D,{D,�{D-�D-�3D.HD.�{D/D/��D0{D0�{D1fD1��D2D2�{D3�D3��D4�D4��D5{D5�D6�D6��D7�D7�3D8fD8�fD9�D9�HD:HD:��D;�D;��D<fD<��D=3D=��D>D>��D?{D?�D@�D@��DA3DA��DBDB�DCHDC��DD�DD�DE3DE��DF�DF�DGDG��DHDH�{DI{DI�3DJ3DJ�{DK�DK��DL�DL�HDM�DM��DN{DN��DO�DO�3DP�DP��DQ3DQ�{DR{DR�3DS{DS�{DT�DT��DU{DU��DV�DV�3DW�DW��DX�DX��DYHDY��DZ�DZ��D[3D[�3D\�D\�HD]HD]�HD^�D^��D_�D_�{D`�D`��Da3Da��Db3Db��Dc3Dc��Dd{Dd��De�De�
Df
Df�{DgDg�fDhDh�{DiDi��DjHDj��Dk{Dk��Dl�Dl�Dm�Dm��Dn�Dn�HDo�Do�{Dp3Dp��Dq{Dq��Dr�Dr��Ds �Ds�3Dt�Dt��Du�Du��DvDv��Dw3Dw��Dx�Dx��Dy�Dy��Dz�Dz�HD{{D{�
D|
D|��D}
D}��D~3D~��D{D�D��D�@�D���D���D� �D�A�D���D��HD��D�B=D���D��HD�HD�B=D��=D���D��D�A�D��HD��HD�=D�B�D���D���D��D�B�D���D��HD�HD�A�D��=D��=D�HD�@�D��HD��HD��D�A�D��=D���D��D�A�D���D���D��D�B�D��HD���D��D�A�D��HD���D��D�C3D���D���D��D�B=D��HD��HD�HD�AHD���D���D��D�B�D���D���D� �D�@�D���D��3D��D�A�D���D���D� �D�A�D���D��HD��D�B=D���D���D��D�A�D��HD��HD�HD�@�D���D�D�3D�B=D��=D���D� �D�B=D���D���D��D�A�D���D���D��D�B=D���D���D��D�A�D��HD��=D�=D�B=D���D���D��D�B�D���D���D�=D�B�D��3D�D��D�A�D���D���D��D�AHD��HD���D�HD�@�D��HD���D��D�AHD���D���D�=D�B=D���D�D��D�A�D���D���D��D�B�D���D��=D��D�B=D��=D��HD��D�B=D���D���D� �D�AHD���D���D��D�B=D��3D�D�=D�B=D��=D�D��D�A�D���D��3D��D�A�D���D���D��D�B�D���D�D�=D�B=D��=D��=D�HD�AHD���D��=D�=D�A�D��=D��HD� �D�@�D��HD��=D�=D�A�D���D��HD�HD�AHD��HD���D�HD�AHD��=D��=D�HD�@�D���D���D��D�A�D���D��=D��D�A�D���D���D� �D�AHD���D�D��D�B=D���D���D�3D�C3D���D��=D��D�B=D���D��=D� �D�@�D���D��=D�=D�B=D���D��=D��D�A�D���D��=D��D�C3D���D���D�HD�A�D��=D��HD� �D�A�D���D�D�HD�AHD�D���D� RD�B=DÂ�D���D� �D�@�DĀ�D���D�HD�A�Dł=D���D��D�C�Dƃ3D�D�3D�B�Dǃ3D���D��D�B�DȂ�D���D� �D�@�DɁHD���D��D�A�Dʁ�D��=D�HD�A�Dˁ�D���D��D�B�D̂�D��HD�=D�C3D̓3D�D��D�A�D΁HD��HD��D�A�Dρ�D�D� �D�AHDЁ�D���D��D�A�Dс�D���D��D�B�D҂�D��HD��D�B�DӁ�D���D��D�B=DԂ�D���D�HD�B=DՂ�D�D��D�B=Dց�D��HD� �D�@�Dׂ�D���D� �D�A�D؁�D���D�=D�AHDـ�D���D� �D�AHDځ�D��=D��D�B=Dہ�D���D��D�B�D܁�D���D��D�A�D݁�D��=D��D�A�Dށ�D���D�HD�A�D߁�D���D�=D�B�D��=D��=D��D�B�D�=D�D�=D�B=D₏D�D�=D�AHD��D���D��D�B�D䂏D���D��D�B�D��D��RD� �D�AHD��D���D��D�B�D�=D���D��D�B=D��D��HD�HD�B=D��D��=D��D�B=D��D��=D� �D�@�D끚D��HD� �D�@�D�HD��=D��D�A�D큚D���D�HD�B=DD��=D��D�B�DD���D�HD�@�D���D���D� �D�A�D�D��HD��D�AHD��D���D��D�A�D�HD��HD��D�B=D�=D�D��D�A�D���D���D��D�AHD���D���D��D�B�D���D���D��D�B=D��HD���D� �D�A�D���D��HD��D�B=D���D��3D��D�3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�+A��AվwAթ�AՋ�A�qvA�m�A�kQA�c�A�cTA�b�A�bA�b�A�a�A�_�A�_�A�Z�A�TaA�JXA�/�AԓAӱ�A�qvA�a�A�XEA�J#A�:*A�-CA�	A�\A�A��A�wfA��A��A�[�A�P�A��jA̵A��PA��DA�Y�A�]�A���A��mAʿ�A�cTA�˒A��A�4nAǈ1A�x�A�OAřeA��vAĢhAÉ7A�oiA��A���A��EA��IA���A�U�A���A�1'A�M�A��kA�XEA�;�A�w2A��MA���A��HA�GA�m�A��A�ZQA���A�ҽA��A�OA�+A�($A��cA� �A���A��qA�v�A�|�A���A��A�9XA�v`A�pA��/A��A��A�@�A~1'Au��Aqg8Ap|�Ao��An�0Ak�Af��AcxA^c AV�+AT
�ARw2AOQAL�AJ��AI]�AH1'AF�6AC�eAA��A?�A=+A<C�A;��A7��A57LA2rGA0=qA.��A-]dA,�A,�FA,�A*U�A(�A'A�A&{JA%x�A#��A"��A"�DA"$�A ��A ��A �"A ��A _A�AB�A@A4�AA��AMAu�A iA��A��A'RA��A�A�A�A%A�4A\�A�)A��A~�A�Ac�A1'A�A��Ah
AH�A�A��A~�Ae�AC�A \A��A�A�PAxA
��A
\�A	�A�A�A�nA��A�A�!A�0AI�A#:AGA�6A�vA*�A>�A`BA��A�^A��A�A�LA�YAAd�A7LA_AhsA-�Aw2A��A�oAC�AjA ~(@��@��m@��@�|@��0@�ȴ@�K^@��@���@��y@��r@�/�@��>@��>@���@��@�_�@�"h@��@�@�w@�@@�F�@���@��'@��@�'R@�ی@�$�@��r@��&@���@�c�@�1@��3@�2a@�'@��[@��"@�YK@�)�@�˒@�)_@�ȴ@�u�@�-�@��@��;@�l�@�+@�%F@��@�@��@��@��@�H@ݓ@�'R@ڌ�@�ݘ@�+�@���@��f@��8@��E@���@تe@آ4@�i�@�Ft@٩�@���@�!�@���@ח$@�N<@���@�'R@�X�@Ԣ4@Ӹ�@�IR@��@��m@�_�@�C-@��@�C�@���@��c@��@ΐ.@ͮ�@�`B@ͼ@͘�@���@̆Y@�0U@�\)@�IR@�(�@��/@��j@�%F@ȸR@�r�@�a|@��@�s�@�@@��,@�_�@���@��@��@Š'@�<6@��E@��+@Ê	@�+@��8@F@�_@��.@���@�6@���@��@�zx@���@���@�d�@�(�@���@��6@��	@��_@�z@�e@��~@�)_@��@�ی@���@���@�@�J#@��@�<�@���@�B�@��B@�tT@�V�@��@��'@�y�@�F@��@���@�c�@��>@��3@���@�~�@��@���@��1@�{�@�bN@�Z@�Ov@�7@���@���@�O@��@�ȴ@��@��x@��@���@�u%@�\�@��m@�Dg@��@�~�@���@��@�f�@��2@���@�@��t@�zx@�L�@��E@���@�x@�4@�0U@��k@� i@���@�$�@���@��@��@��@���@��M@��M@��4@�|@�rG@�b�@��@��4@��[@�C@���@��I@�s�@��@��F@�x�@�U�@���@��[@���@�a|@�Ov@�(�@���@��@���@�W?@���@���@���@���@�e�@�>B@�@���@�Vm@�5�@��@��@�V@�;@��B@���@���@�q�@�C-@�1@��-@��{@�?}@��E@�C�@���@�x�@�4@��@��/@���@��9@���@�c @��@�G@��&@��0@��=@�S�@��@���@��I@�M@��@���@���@�F�@�0�@�#�@��@��1@��@���@��"@�X�@��@�ں@�͟@�Ta@�{@��6@�|@�X�@�9�@�"�@�%@��@��/@��E@��$@���@�l"@��@�|�@�j@�Y�@�+@���@�:�@��t@�X�@�C@��K@�g8@��@���@�"�@�bN@���@�|@�2a@���@�ȴ@���@�?@��V@�qv@�:�@�7L@� i@��s@���@�j@�c @�6�@��6@���@��7@�"�@���@�n�@�Ft@�'R@�]@��@H�@�@~�2@~�<@~��@~�6@~h
@~.�@}��@}Q�@|�D@|�@{J#@{�@zl�@y�h@y;@x��@w��@w,�@vߤ@v��@vv�@vZ�@vTa@vV@vW�@vL0@u�9@uQ�@toi@s��@r��@r8�@qԕ@q|@q�@p�p@p��@p�o@pr�@pc�@p:�@o�$@o@n_�@m�@m�C@m|@me,@l�/@l`�@l�@kF�@j�@j?@i�@i=�@hw�@g�@f1�@e�9@ec@d�f@d4n@c�a@c8@c�@b��@a�@a�@`�j@`��@`�o@`x@_�@@_{J@_b�@^�@^��@^8�@]��@\�/@\K^@\b@[�W@[�a@[�:@[_p@[33@Z� @Z�@Y\�@X��@Xr�@XZ@X>B@X2�@X~@W�A@WdZ@W'�@V�@V�R@Vs�@V{@U8�@T�@T�z@S�Q@S,�@R�8@R�c@R��@R�@RE�@Q@Q��@Qs�@QIR@P�f@P  @O|�@O,�@N��@N��@N�6@N�@Nxl@NQ@N.�@M�@L�`@L�O@Lj@L�@K��@K��@K�P@K]�@J�@J{�@J�@I��@I \@HZ@G�[@G+@F�y@F�,@F�b@FB[@E��@E��@E��@Es�@EN<@E%F@D��@D��@D��@D��@D_@DG@C�[@C��@C��@CS�@C+@B҉@Bl�@B_@A�-@A|@A�@@��@@��@@�@@`�@@%�@?خ@?��@?Mj@?�@>�H@>��@>�A@>n�@>J�@=��@=��@=w2@=S&@=#�@<�)@<c�@<  @;��@;C�@;Y@;@:�h@:��@:��@:l�@:R�@9�D@9a�@9@8��@8�4@8Xy@8@7�W@7�@7�m@7��@7�@7�k@7@O@6��@6��@6��@6d�@60U@5��@5�-@5\�@4�I@3�@3RT@2�+@26�@2�@1�X@1Y�@0�|@0�@0m�@0~@/˒@/�0@/|�@/,�@/�@.�@.�\@. �@-�"@-(�@,�)@,>B@+�@+O@+@*ں@*�!@*��@*��@*l�@*?@*4@)�C@)zx@)^�@)!�@(�@(]d@(6@(x@'�K@'��@'s@'Mj@&��@&�@&�b@&d�@&^5@&^5@&M�@&�@%�=@%`B@%2a@%�@$�5@$�U@$��@$oi@$M@$<�@$*�@$�@#��@#�@#�6@#�F@#�{@#K�@#'�@#@"͟@"c @"J�@"	@!��@!��@!��@!e,@!5�@ ��@ �`@ ��@ ��@ ��@ H@ �@��@��@$t@��@�b@}V@Ov@B[@8�@�@�@�o@�-@a�@%F@��@��@��@Ɇ@�_@l"@U2@�@v`@��@�!@B[@�D@��@�X@��@��@�	@�/@֡@�?@��@��@U2@?�@9X@"h@1@�+@��@�@�Q@��@�$@y�@�@�M@�,@Q@�o@�=@^�@&�@��@�v@��@��@�I@�_@��@h�@I�@D�@<�@!@�@�&@�0@�*@o�@X�@;d@�@��@�@��@��@YK@?@��@�3@��@��@5�@�@��@�@�$@�O@��@~(@@�@�F@�*@�$@�f@iD@\)@�]@��@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�+A��AվwAթ�AՋ�A�qvA�m�A�kQA�c�A�cTA�b�A�bA�b�A�a�A�_�A�_�A�Z�A�TaA�JXA�/�AԓAӱ�A�qvA�a�A�XEA�J#A�:*A�-CA�	A�\A�A��A�wfA��A��A�[�A�P�A��jA̵A��PA��DA�Y�A�]�A���A��mAʿ�A�cTA�˒A��A�4nAǈ1A�x�A�OAřeA��vAĢhAÉ7A�oiA��A���A��EA��IA���A�U�A���A�1'A�M�A��kA�XEA�;�A�w2A��MA���A��HA�GA�m�A��A�ZQA���A�ҽA��A�OA�+A�($A��cA� �A���A��qA�v�A�|�A���A��A�9XA�v`A�pA��/A��A��A�@�A~1'Au��Aqg8Ap|�Ao��An�0Ak�Af��AcxA^c AV�+AT
�ARw2AOQAL�AJ��AI]�AH1'AF�6AC�eAA��A?�A=+A<C�A;��A7��A57LA2rGA0=qA.��A-]dA,�A,�FA,�A*U�A(�A'A�A&{JA%x�A#��A"��A"�DA"$�A ��A ��A �"A ��A _A�AB�A@A4�AA��AMAu�A iA��A��A'RA��A�A�A�A%A�4A\�A�)A��A~�A�Ac�A1'A�A��Ah
AH�A�A��A~�Ae�AC�A \A��A�A�PAxA
��A
\�A	�A�A�A�nA��A�A�!A�0AI�A#:AGA�6A�vA*�A>�A`BA��A�^A��A�A�LA�YAAd�A7LA_AhsA-�Aw2A��A�oAC�AjA ~(@��@��m@��@�|@��0@�ȴ@�K^@��@���@��y@��r@�/�@��>@��>@���@��@�_�@�"h@��@�@�w@�@@�F�@���@��'@��@�'R@�ی@�$�@��r@��&@���@�c�@�1@��3@�2a@�'@��[@��"@�YK@�)�@�˒@�)_@�ȴ@�u�@�-�@��@��;@�l�@�+@�%F@��@�@��@��@��@�H@ݓ@�'R@ڌ�@�ݘ@�+�@���@��f@��8@��E@���@تe@آ4@�i�@�Ft@٩�@���@�!�@���@ח$@�N<@���@�'R@�X�@Ԣ4@Ӹ�@�IR@��@��m@�_�@�C-@��@�C�@���@��c@��@ΐ.@ͮ�@�`B@ͼ@͘�@���@̆Y@�0U@�\)@�IR@�(�@��/@��j@�%F@ȸR@�r�@�a|@��@�s�@�@@��,@�_�@���@��@��@Š'@�<6@��E@��+@Ê	@�+@��8@F@�_@��.@���@�6@���@��@�zx@���@���@�d�@�(�@���@��6@��	@��_@�z@�e@��~@�)_@��@�ی@���@���@�@�J#@��@�<�@���@�B�@��B@�tT@�V�@��@��'@�y�@�F@��@���@�c�@��>@��3@���@�~�@��@���@��1@�{�@�bN@�Z@�Ov@�7@���@���@�O@��@�ȴ@��@��x@��@���@�u%@�\�@��m@�Dg@��@�~�@���@��@�f�@��2@���@�@��t@�zx@�L�@��E@���@�x@�4@�0U@��k@� i@���@�$�@���@��@��@��@���@��M@��M@��4@�|@�rG@�b�@��@��4@��[@�C@���@��I@�s�@��@��F@�x�@�U�@���@��[@���@�a|@�Ov@�(�@���@��@���@�W?@���@���@���@���@�e�@�>B@�@���@�Vm@�5�@��@��@�V@�;@��B@���@���@�q�@�C-@�1@��-@��{@�?}@��E@�C�@���@�x�@�4@��@��/@���@��9@���@�c @��@�G@��&@��0@��=@�S�@��@���@��I@�M@��@���@���@�F�@�0�@�#�@��@��1@��@���@��"@�X�@��@�ں@�͟@�Ta@�{@��6@�|@�X�@�9�@�"�@�%@��@��/@��E@��$@���@�l"@��@�|�@�j@�Y�@�+@���@�:�@��t@�X�@�C@��K@�g8@��@���@�"�@�bN@���@�|@�2a@���@�ȴ@���@�?@��V@�qv@�:�@�7L@� i@��s@���@�j@�c @�6�@��6@���@��7@�"�@���@�n�@�Ft@�'R@�]@��@H�@�@~�2@~�<@~��@~�6@~h
@~.�@}��@}Q�@|�D@|�@{J#@{�@zl�@y�h@y;@x��@w��@w,�@vߤ@v��@vv�@vZ�@vTa@vV@vW�@vL0@u�9@uQ�@toi@s��@r��@r8�@qԕ@q|@q�@p�p@p��@p�o@pr�@pc�@p:�@o�$@o@n_�@m�@m�C@m|@me,@l�/@l`�@l�@kF�@j�@j?@i�@i=�@hw�@g�@f1�@e�9@ec@d�f@d4n@c�a@c8@c�@b��@a�@a�@`�j@`��@`�o@`x@_�@@_{J@_b�@^�@^��@^8�@]��@\�/@\K^@\b@[�W@[�a@[�:@[_p@[33@Z� @Z�@Y\�@X��@Xr�@XZ@X>B@X2�@X~@W�A@WdZ@W'�@V�@V�R@Vs�@V{@U8�@T�@T�z@S�Q@S,�@R�8@R�c@R��@R�@RE�@Q@Q��@Qs�@QIR@P�f@P  @O|�@O,�@N��@N��@N�6@N�@Nxl@NQ@N.�@M�@L�`@L�O@Lj@L�@K��@K��@K�P@K]�@J�@J{�@J�@I��@I \@HZ@G�[@G+@F�y@F�,@F�b@FB[@E��@E��@E��@Es�@EN<@E%F@D��@D��@D��@D��@D_@DG@C�[@C��@C��@CS�@C+@B҉@Bl�@B_@A�-@A|@A�@@��@@��@@�@@`�@@%�@?خ@?��@?Mj@?�@>�H@>��@>�A@>n�@>J�@=��@=��@=w2@=S&@=#�@<�)@<c�@<  @;��@;C�@;Y@;@:�h@:��@:��@:l�@:R�@9�D@9a�@9@8��@8�4@8Xy@8@7�W@7�@7�m@7��@7�@7�k@7@O@6��@6��@6��@6d�@60U@5��@5�-@5\�@4�I@3�@3RT@2�+@26�@2�@1�X@1Y�@0�|@0�@0m�@0~@/˒@/�0@/|�@/,�@/�@.�@.�\@. �@-�"@-(�@,�)@,>B@+�@+O@+@*ں@*�!@*��@*��@*l�@*?@*4@)�C@)zx@)^�@)!�@(�@(]d@(6@(x@'�K@'��@'s@'Mj@&��@&�@&�b@&d�@&^5@&^5@&M�@&�@%�=@%`B@%2a@%�@$�5@$�U@$��@$oi@$M@$<�@$*�@$�@#��@#�@#�6@#�F@#�{@#K�@#'�@#@"͟@"c @"J�@"	@!��@!��@!��@!e,@!5�@ ��@ �`@ ��@ ��@ ��@ H@ �@��@��@$t@��@�b@}V@Ov@B[@8�@�@�@�o@�-@a�@%F@��@��@��@Ɇ@�_@l"@U2@�@v`@��@�!@B[@�D@��@�X@��@��@�	@�/@֡@�?@��@��@U2@?�@9X@"h@1@�+@��@�@�Q@��@�$@y�@�@�M@�,@Q@�o@�=@^�@&�@��@�v@��@��@�I@�_@��@h�@I�@D�@<�@!@�@�&@�0@�*@o�@X�@;d@�@��@�@��@��@YK@?@��@�3@��@��@5�@�@��@�@�$@�O@��@~(@@�@�F@�*@�$@�f@iD@\)@�]@��@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bw�Bv�Bv�BvzBvBu�Bu�ButBuZBu�Bu�Bu�Bu�Bu�Bv`Bv�Bv�Bv�BvzBvBt�Bp�Bi�Bh$Bg�Bh>BiBi�Bi�Bi*Bh�BiyBl�B��B�BB��B��B�mB�UB	n}B	��B
dZB
}"B
s�B
��B
��B
�jB
��B
��B"B'mB8RB:*BC-Bd�Bl�BmCB��B�B��B�B�}B�B�	B�B�vB�B��B�B�B�B�BB �B��B�tB�B�B�[B�RB��B�EB��Bm)B`'B\)BP}B7�BqBB
�LB
�B
�bB
�)B
��B
��B
�B
u%B
h�B
C�B

#B	�mB	�8B	��B	��B	��B	�_B	�%B	t�B	a�B	AB	88B	0�B	(�B	"4B	"�B	(
B	+QB	*0B	+kB	(�B	.}B	*eB	%FB	 �B	/B	]B	 BB	#:B	%zB	,�B	0oB	1[B	3�B	;�B	>�B	?B	A�B	F�B	KB	I�B	SuB	[=B	d&B	jB	n�B	rGB	v�B	y�B	s�B	w�B	x8B	v`B	x8B	}<B	��B	��B	�IB	�vB	�5B	��B	�LB	��B	��B	��B	�zB	��B	��B	�CB	��B	�]B	��B	�B	��B	�VB	�B	��B	��B	�VB	�<B	�BB	��B	��B	�"B	�B	�qB	�wB	��B	��B	�"B	��B	�<B	�B	�VB	��B	��B	�;B	�'B	��B	��B	ȀB	��B	бB	�B	רB	چB	�BB	�hB	�,B	��B	�B	��B	��B	�B	��B	�WB	�]B	�oB	�B	��B	��B	�qB	�mB	�|B	�/B	�B	��B	�oB	��B	�FB	�,B	�B	�2B	��B	�9B	��B	�5B	�jB	��B	��B	��B	�B	�FB	�B	�`B	�,B	�FB	�B	�LB	�B	�RB	�B	�2B	�B	��B	��B	��B	�DB	��B	�XB	��B	�
B	��B	�RB	��B	�B	�KB	�6B	�QB	�kB	�B	��B	��B	�B	��B	�B	��B	�mB	�B	�B	�fB	�zB	��B	�TB	�B	��B	�B	�B	�B	��B	�B	�B	�$B	�B	��B
 B
 B	��B	�jB	�B	�qB	��B	��B	��B	��B	��B	�tB	�?B	�hB	��B	��B	��B	�)B	�OB	�B	�B	�B	�B	�XB	�B	�B	�	B	��B	�B	�6B	��B	��B	�6B	�PB	��B
�B
�B
SB
�B
SB
9B
�B
mB
mB
B
B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
KB
�B
B
	B
	B
	B
	�B
	lB
	lB
	B
	B
B
�B
KB
�B
1B
fB
1B
KB

	B
�B
�B
�B
�B
DB

rB

XB

�B
~B
�B
�B
�B
�B
B
�B
�B
�B
�B
<B
B
�B
(B
BB
BB
BB
BB
�B
�B
�B
�B
�B
�B
bB
.B
.B
�B
�B
�B
B
�B
�B
\B
�B
�B
JB
B
dB
6B
PB
�B
jB
�B
"B
�B
pB
�B
}B
NB
�B
:B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
@B
�B
MB
�B
�B
B
mB
�B

B
�B
YB
sB
+B
B
�B
B
+B
�B
B
�B
=B
WB
=B
qB
�B
�B
�B
]B
/B
/B
~B
dB
~B
dB
�B
B
B
�B
�B
B
�B
�B
�B
5B
�B
 �B
 �B
!�B
"B
"B
"B
"NB
"�B
# B
#nB
#TB
#�B
#�B
#�B
#�B
$�B
$tB
$tB
%`B
%,B
%FB
%zB
%`B
%`B
%,B
%,B
%FB
&LB
'B
&�B
'B
'8B
'B
'B
'�B
'�B
(>B
(sB
(�B
(�B
(�B
)B
)DB
)*B
)*B
)_B
)�B
)_B
*eB
*�B
*�B
*�B
+B
+�B
,B
,�B
,�B
-B
-)B
.B
.�B
.�B
/B
0!B
0oB
1B
1'B
1vB
1�B
1�B
1�B
2�B
2�B
3B
2�B
3MB
33B
3�B
3�B
3�B
3�B
33B
2�B
2�B
2�B
3hB
3�B
3�B
4B
49B
4�B
4�B
5B
5%B
5ZB
5ZB
5ZB
5�B
5�B
5�B
6zB
7B
72B
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
9$B
9>B
9rB
9rB
9rB
9rB
9>B
9>B
9�B
9�B
:^B
;dB
;�B
<jB
<�B
="B
=VB
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
?B
>�B
?}B
?�B
?�B
@iB
@�B
AB
AB
A�B
A�B
BuB
C{B
C�B
C�B
C�B
D�B
D�B
EB
EB
E9B
F?B
FYB
F�B
F�B
F�B
GB
GEB
G_B
GEB
G�B
GzB
G�B
G�B
H�B
IB
IB
IB
IB
IRB
IRB
IRB
J	B
JXB
J�B
KxB
KxB
KxB
K�B
KxB
K^B
KxB
K�B
K�B
L0B
L0B
LJB
L~B
MPB
MPB
MjB
NB
NpB
N�B
N�B
N�B
N�B
OB
O\B
OBB
OvB
OvB
O�B
PbB
Q B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
R�B
R�B
R�B
S[B
S@B
S[B
SuB
S�B
S�B
TB
T{B
T�B
T�B
T�B
T�B
UgB
UgB
UgB
UgB
U�B
U�B
U�B
U�B
U�B
VB
V9B
V�B
V�B
V�B
V�B
W$B
W?B
W�B
W�B
XEB
XEB
X_B
X�B
X�B
X�B
X�B
Y1B
YeB
Y�B
Y�B
ZB
ZQB
Z7B
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[WB
[WB
[qB
[�B
\)B
\]B
\�B
]B
]B
]B
]IB
]dB
]dB
]~B
]dB
]�B
^jB
^�B
^�B
_!B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`B
`\B
`�B
`�B
`�B
`�B
aB
a-B
abB
abB
a�B
a�B
bNB
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
dZB
d@B
d�B
d�B
d�B
d�B
d�B
eB
e�B
e�B
fB
f�B
f�B
gRB
gRB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
h�B
h�B
h�B
h�B
i�B
i�B
jKB
jeB
j�B
j�B
j�B
kB
kkB
kkB
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
mB
mB
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
n�B
n�B
n�B
oB
oOB
oOB
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
qB
qB
q'B
q[B
q�B
q�B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
tB
t9B
tB
t�B
uB
utB
u�B
u�B
vFB
v`B
v`B
v`B
v`B
v�B
v�B
v�B
wB
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x�B
xlB
x�B
y$B
yrB
y�B
y�B
zB
z*B
z*B
zDB
z^B
z^B
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{0B
{JB
{B
{�B
|B
|B
|B
|6B
|PB
|PB
|�B
|�B
|�B
}"B
}�B
~B
~B
~(B
~(B
~]B
~]B
~]B
~�B
~�B
B
HB
cB
cB
}B
cB
� B
�4B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bw�BwBwBv�BvFBv+Bu�ButBuZBu�Bu�Bu�Bu�Bu�Bv`Bv�Bv�Bv�Bv�Bv�BvBr-BjKBh>Bg�BhXBi*Bi�Bi�BiDBh�Bi�Bm�B��B�hB�>B�+B�B�TB	n�B
 OB
d�B
~wB
t�B
�JB
��B
�qB
�4B
�B�B(�B8�B;�BE�Be�Bm�Bo�B�mB��B�B��BĜBňB�]B�'B�ZB�B��BzB	lBB�B�BB�B�XB�=B�nB�B��B��B��B��BoOBa�B_BU�B<PB�B�B
�XB
��B
�B
�"B
��B
��B
�B
y	B
pUB
NB
oB	ڠB	��B	��B	��B	�zB	�~B	�DB	{�B	i_B	D�B	:�B	4nB	+�B	$�B	$�B	)�B	-�B	-�B	.IB	+�B	0�B	+�B	'B	%�B	 �B	�B	"�B	%,B	&�B	-CB	1B	2�B	6+B	>(B	?�B	@OB	C-B	H�B	K�B	J�B	TFB	\]B	d�B	j0B	o B	sB	x�B	{B	utB	xlB	x�B	v�B	y	B	~wB	�QB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�@B	�_B	�CB	��B	��B	�5B	�MB	�XB	��B	�<B	��B	�<B	��B	�qB	�wB	��B	�B	��B	�VB	�(B	�B	�B	��B	�B	�(B	�qB	�<B	��B	��B	��B	��B	�[B	�-B	��B	�KB	��B	�}B	��B	�sB	�QB	�'B	�NB	�zB	�$B	�_B	�B	�0B	��B	��B	��B	�cB	�AB	��B	�B	�3B	��B	�sB	�NB	��B	�IB	��B	�B	�,B	ԕB	ԕB	�{B	�gB	�MB	�SB	��B	�jB	��B	��B	�B	�|B	�TB	�zB	�B	�B	�`B	�`B	�LB	�B	�mB	�B	�B	�fB	��B	��B	�>B	�B	��B	��B	�B	��B	�sB	�B	�B	�XB	��B	�B	�QB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�8B	��B	�mB	�fB	�tB	�B	�B	��B	�,B	�B	�B	��B	�B	�B	��B	�yB	�B
�B
 �B	�(B	��B	�PB	�B	�BB	�B	�DB	��B	��B	��B	�tB	�B	�B	��B	��B	�B	�B	�[B	�B	��B	��B	��B	��B	�^B	�XB	�^B	�6B	�jB	��B	�(B	��B	��B
  B
�B
�B
�B
�B
�B
mB
�B
mB
�B
SB
mB
B
�B
B
SB
�B
�B
9B
YB
zB
	B
�B
fB
1B
�B
	RB
	7B
	RB
	�B
	�B
	�B
	�B
	7B
fB
	B
�B
�B
fB
�B
fB
�B

�B
�B
0B
B
JB
�B

�B

rB
DB
�B
�B
B
B
�B
PB
�B
�B
�B
B
�B
(B
�B
BB
\B
\B
\B
\B
�B
�B
B
B
�B
�B
}B
�B
�B
�B
�B
4B
�B
 B
bB
�B
BB
pB
�B
dB
�B
PB
�B
�B
�B
VB
pB
�B
(B
bB
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
@B
�B
MB
gB
�B
B
SB
�B

B
?B

B
sB
�B
+B
+B
�B
EB
EB
�B
KB
B
WB
WB
=B
�B
�B
B
B
�B
IB
IB
�B
~B
~B
�B
B
B
B
B
B
B
�B
�B
B
�B
�B
 �B
!-B
!�B
"B
"4B
"B
"hB
"�B
#TB
#�B
#nB
#�B
#�B
#�B
$@B
$�B
$�B
$�B
%zB
%FB
%zB
%�B
%`B
%zB
%FB
%`B
%�B
&�B
'8B
'B
'8B
'8B
'8B
'RB
'�B
($B
(XB
(�B
(�B
(�B
(�B
)*B
)_B
)*B
)*B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+kB
,B
,qB
,�B
-)B
-)B
-wB
.}B
.�B
/5B
/�B
0UB
0�B
1'B
1[B
1�B
1�B
1�B
2GB
2�B
2�B
33B
2�B
3hB
3MB
3�B
3�B
3�B
4B
33B
3B
2�B
3B
3�B
3�B
4B
49B
4nB
4�B
4�B
5B
5%B
5ZB
5ZB
5ZB
5�B
5�B
6B
6�B
7LB
7�B
8B
7�B
8�B
8�B
8�B
9	B
8�B
8�B
9>B
9XB
9rB
9rB
9XB
9rB
9>B
9XB
9�B
9�B
:�B
;�B
;�B
<�B
<�B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>BB
>�B
>�B
?B
?B
?B
?�B
?�B
@B
@�B
AB
A B
AUB
A�B
BAB
B�B
C�B
C�B
C�B
DMB
D�B
D�B
E9B
E9B
E�B
FYB
FtB
F�B
F�B
F�B
G+B
GEB
G_B
GzB
G�B
G�B
G�B
H1B
IB
IB
I7B
I7B
IB
IlB
IlB
I�B
J=B
J�B
J�B
KxB
KxB
KxB
K�B
KxB
K^B
K�B
K�B
K�B
LJB
LJB
L~B
L�B
MjB
M�B
M�B
N"B
N�B
N�B
N�B
N�B
N�B
OBB
OvB
OBB
OvB
O�B
O�B
P�B
QB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RTB
R�B
R�B
R�B
R�B
R�B
SuB
SuB
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
UB
UgB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
VB
VB
VSB
V�B
V�B
V�B
V�B
W$B
WYB
W�B
W�B
XEB
X_B
X_B
X�B
X�B
X�B
YB
YKB
YB
Y�B
Y�B
Z7B
ZkB
Z7B
ZkB
Z�B
Z�B
Z�B
[#B
[=B
[WB
[WB
[�B
[�B
\]B
\xB
\�B
]B
]/B
]/B
]dB
]~B
]dB
]~B
]�B
^B
^�B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
_�B
`B
_�B
`B
`vB
`�B
`�B
`�B
aB
a-B
aHB
a|B
a�B
bB
b4B
b�B
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
dtB
dZB
d�B
d�B
d�B
d�B
d�B
eFB
e�B
fB
fLB
f�B
f�B
gmB
gmB
g�B
g�B
g�B
g�B
g�B
h$B
h>B
h�B
h�B
h�B
i*B
i�B
i�B
jKB
jB
j�B
kB
j�B
k6B
kkB
kkB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
m)B
m)B
m]B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nIB
nIB
n}B
n�B
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
pB
pB
pUB
p�B
p�B
p�B
p�B
p�B
q'B
q'B
q'B
q�B
rB
rB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
t�B
u?B
utB
u�B
vB
v`B
v`B
v`B
v`B
v�B
v�B
v�B
v�B
w2B
wB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x�B
xlB
x�B
y>B
y�B
y�B
zB
z*B
z*B
z*B
z^B
zDB
z^B
z^B
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{B
z�B
{JB
{JB
{dB
{�B
{�B
|B
|6B
|B
|PB
|PB
|PB
|�B
|�B
}B
}<B
}�B
~B
~B
~(B
~(B
~]B
~wB
~wB
B
~�B
B
.B
cB
}B
}B
}B
� B
�4B
�4311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905240055262019052400552620190524005526202207271131082022072711310820220727113108202207271533512022072715335120220727153351  JA  ARFMdecpA30a                                                                20190523095842  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100032  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100032  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100033  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100033  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100033                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111515                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190514000000  CF  PSAL_ADJUSTED_QC@6ff@6ffG�O�                JM  ARCAJMQC2.0                                                                 20190523155526  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190523155526  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023108  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063351  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                