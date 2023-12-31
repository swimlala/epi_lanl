CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-22T15:37:33Z creation;2019-06-22T15:37:37Z conversion to V3.1;2019-12-18T07:14:18Z update;2022-11-21T05:28:46Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20190622153733  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_177                     2C  DdL�NAVIS_A                         0397                            ARGO 011514                     863 @�ǣ4��1   @�ǣ�6� @;���{���dL���D�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�
=A�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C!HC�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D��D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�
=A�JA�
=A�%A�%A�1A�
=A�VA���A��A�dZA×�A�{A���A�l�A�O�A�-A�"�A�=qA���A�M�A�VA�ƨA�l�A�z�A�t�A�O�A���A��\A�S�A��FA�O�A�|�A�?}A�VA�`BA�t�A�A��RA���A�^5A��
A�XA�oA��#A���A�/A��A�A�A��A��A�oA�\)A�z�A��A���A��HA���A��A�ȴA�p�A��^A�ƨA��!A�VA~Q�A|ȴA{��A{��A{p�Ay�wAw�hAt��AtJAr�jAq�;AqoAp�uAo&�Am�AjVAhbNAfv�Ad^5AcVAbz�Aa�hA`�\A`JA^��A]ƨA\z�A[�AZA�AY�AY7LAXbNAWK�AU�TAUx�ATȴAS�AR��ARM�APv�AN5?AM��AL�AJn�AI
=AHȴAH�jAHn�AG�wAFffAF=qAF1AE��AEx�AEVAD �AA��A@5?A>�+A=7LA<Q�A;�-A;O�A:��A:I�A:9XA9ƨA9XA8��A8=qA6��A6VA6�A4�`A4z�A4�A3�mA3�-A3�A0��A/�hA/oA.��A.A-��A-C�A,ĜA,A+\)A*�jA*ZA*�A)�A)oA(�!A(�+A(n�A(^5A'dZA&�uA%��A%%A${A!hsA�AA��A^5A�AĜA�wA�FA�A%A�A5?A�A�hA&�A�/AAt�A �A"�A5?A  A�^A��AC�A�A��A"�A1AhsA�HAn�A�A
Q�A�\A�#A�A��A��A
=AVA�^AC�AA�A��A�+A�^A�A�9A=qAS�A b@��T@��@�;d@�@�`B@���@��@��@���@��/@��@�F@��@�9@���@�j@��m@�x�@��y@���@��`@�r�@�l�@݁@�t�@ڸR@�~�@�J@٩�@�O�@�G�@��@���@�Ĝ@ؓu@�9X@��;@׾w@�|�@�`B@��;@��H@�@ύP@�=q@��@�@͙�@�/@��@ɩ�@Ǿw@��@�z�@�5?@�%@��u@��F@���@��\@�^5@�V@�-@�p�@�Ĝ@�bN@�(�@���@�t�@��@���@�Z@���@���@�@���@�J@�&�@�j@��y@��@��@���@��@�1@�o@��^@��@�;d@���@�p�@�?}@���@��@��m@�o@�V@��
@��@�l�@�dZ@��+@��T@���@�G�@��`@���@�j@���@���@�33@�ff@���@��#@���@�O�@��@��@��j@�bN@�ƨ@�+@���@�ȴ@�n�@��@���@�hs@�G�@��@���@�;d@��@��@���@���@�^5@�-@��T@���@�X@�?}@�&�@��`@�A�@��F@�|�@�;d@���@��@���@�v�@��-@�&�@��/@���@��@�(�@�dZ@��R@�-@�@�@��7@�x�@�&�@��D@��@��@���@��w@��@�;d@�o@��@���@��@���@�z�@�Q�@��@�ƨ@�l�@��H@�M�@���@���@�`B@�/@��@��j@�1'@��F@���@���@��P@�t�@�K�@�33@�o@���@�n�@�E�@�$�@���@��h@�O�@�G�@�7L@�7L@�&�@��@�%@���@��9@��u@�j@�A�@�b@|�@�@~�R@~��@~v�@~V@}�@}�-@}�@}/@|�j@|Z@{��@{��@{33@zM�@y�@y7L@y%@xr�@w�@wK�@v�@v�R@v��@vE�@vE�@v5?@v$�@v@u��@u?}@uV@t�@t�j@tz�@t1@s�F@sS�@r��@q��@q7L@q%@p�`@pĜ@p�9@p��@p1'@n�y@nȴ@n�R@nV@m�@l�@lI�@l�@k�m@kƨ@k�F@k��@k��@kt�@j�!@i�7@i&�@i�@h��@hb@gK�@g�@f��@f{@e@e��@d��@d��@d�@dj@d(�@d1@c��@c��@co@b�!@b~�@a��@a�@a�7@a7L@`�9@`bN@_�@_;d@^��@^�+@^V@^V@^5?@^5?@^5?@^$�@]�T@]�-@]/@\�@\�/@\��@\��@\j@[t�@Z�H@Z��@Z~�@Z�@ZJ@ZJ@ZJ@Y��@Y��@XA�@V��@VE�@V@U�-@U�@UO�@UV@T��@T9X@Sƨ@S�F@S��@St�@SC�@S@R�@R��@R��@R��@R�\@Rn�@Rn�@Rn�@R^5@R-@R-@Q��@Q��@Q7L@P��@PA�@PA�@Pb@O��@O;d@Nȴ@N��@N��@Nv�@NV@NE�@N{@M�h@M?}@L��@Lz�@LI�@L(�@L(�@K��@K�m@Kƨ@K��@K�@KdZ@K"�@J��@J�@I�7@I%@HĜ@HA�@G��@GK�@F��@E�T@E?}@D�D@D1@C�F@CS�@B�@B~�@A��@AX@A�@@�`@@��@@r�@@b@?��@?��@?l�@?
=@>�R@>�R@>��@>{@=@=`B@<��@<�@<�@<�D@;��@;��@;S�@:�@:�!@:-@9�#@9x�@8�9@8Q�@7��@7|�@7\)@7;d@6ȴ@6V@65?@5�@5p�@4�@4�/@4�j@4�@49X@3��@2��@2n�@2^5@2^5@2M�@2-@1��@1�#@1��@1��@1��@1X@1�@0��@0�9@0�@0A�@0 �@0b@0 �@0b@/�;@/|�@.�y@.�y@.�y@.ȴ@.V@-��@-�@-/@,��@,��@,�D@,j@,�@+�m@+�
@+ƨ@+��@+C�@+o@*�H@*��@*^5@*M�@*�@)��@)��@)%@(��@(��@(�@(bN@(A�@(  @'��@'K�@'
=@&��@&�y@&�@&�R@&�R@&��@&�+@&E�@&5?@&{@&@%�T@%��@%��@%@%p�@%/@%�@%V@$��@$�@$�@$�D@$9X@#ƨ@#t�@#33@#@"�H@"��@"�\@"�@!�#@!��@!hs@!G�@!&�@!&�@!�@!%@ �`@ bN@�;@��@�P@|�@l�@l�@�@V@$�@{@@�@@O�@�@�/@��@Z@9X@9X@(�@�@��@��@��@��@�\@n�@^5@=q@��@��@�7@hs@G�@�@�u@r�@A�@�;@�P@+@ff@E�@{@�T@�T@��@�h@��@�/@�/@��@�@z�@�m@�F@S�@C�@"�@�!@=q@�@�^@��@hs@&�@��@��@��@��@�@A�@�@�w@��@�P@�P@�P@\)@�y@��@V@V@E�@{@@�T@�-@/@V@V@V@�/@�@�@�D@�D@�D@�D@�D@��@��@��@��@Z@1@t�@S�@"�@@
�@
�@
��@
M�@	��@	��@	��@	x�@	hs@	7L@	�@��@Ĝ@��@Q�@ �@ �@b@��@l�@
=@�@��@E�@{@�T@@�-@�-@�-@�h@`B@�@�j@�@��@z�@Z@I�@9X@(�@�@1@��@�m@�m@�m@�
@�F@��@��@t�@dZ@S�@33@33@33@o@��@�!@~�@M�@=q@�@��@�#@�7@hs@X@7L@7L@�@%@%@ ��@ Ĝ@ �9@ �u@ �@ r�@ r�@ bN@ A�@ 1'@ b?��w?�\)?��?��R?�V?�{?�{?��?��?��?��-?��h?�O�?�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�
=A�JA�
=A�%A�%A�1A�
=A�VA���A��A�dZA×�A�{A���A�l�A�O�A�-A�"�A�=qA���A�M�A�VA�ƨA�l�A�z�A�t�A�O�A���A��\A�S�A��FA�O�A�|�A�?}A�VA�`BA�t�A�A��RA���A�^5A��
A�XA�oA��#A���A�/A��A�A�A��A��A�oA�\)A�z�A��A���A��HA���A��A�ȴA�p�A��^A�ƨA��!A�VA~Q�A|ȴA{��A{��A{p�Ay�wAw�hAt��AtJAr�jAq�;AqoAp�uAo&�Am�AjVAhbNAfv�Ad^5AcVAbz�Aa�hA`�\A`JA^��A]ƨA\z�A[�AZA�AY�AY7LAXbNAWK�AU�TAUx�ATȴAS�AR��ARM�APv�AN5?AM��AL�AJn�AI
=AHȴAH�jAHn�AG�wAFffAF=qAF1AE��AEx�AEVAD �AA��A@5?A>�+A=7LA<Q�A;�-A;O�A:��A:I�A:9XA9ƨA9XA8��A8=qA6��A6VA6�A4�`A4z�A4�A3�mA3�-A3�A0��A/�hA/oA.��A.A-��A-C�A,ĜA,A+\)A*�jA*ZA*�A)�A)oA(�!A(�+A(n�A(^5A'dZA&�uA%��A%%A${A!hsA�AA��A^5A�AĜA�wA�FA�A%A�A5?A�A�hA&�A�/AAt�A �A"�A5?A  A�^A��AC�A�A��A"�A1AhsA�HAn�A�A
Q�A�\A�#A�A��A��A
=AVA�^AC�AA�A��A�+A�^A�A�9A=qAS�A b@��T@��@�;d@�@�`B@���@��@��@���@��/@��@�F@��@�9@���@�j@��m@�x�@��y@���@��`@�r�@�l�@݁@�t�@ڸR@�~�@�J@٩�@�O�@�G�@��@���@�Ĝ@ؓu@�9X@��;@׾w@�|�@�`B@��;@��H@�@ύP@�=q@��@�@͙�@�/@��@ɩ�@Ǿw@��@�z�@�5?@�%@��u@��F@���@��\@�^5@�V@�-@�p�@�Ĝ@�bN@�(�@���@�t�@��@���@�Z@���@���@�@���@�J@�&�@�j@��y@��@��@���@��@�1@�o@��^@��@�;d@���@�p�@�?}@���@��@��m@�o@�V@��
@��@�l�@�dZ@��+@��T@���@�G�@��`@���@�j@���@���@�33@�ff@���@��#@���@�O�@��@��@��j@�bN@�ƨ@�+@���@�ȴ@�n�@��@���@�hs@�G�@��@���@�;d@��@��@���@���@�^5@�-@��T@���@�X@�?}@�&�@��`@�A�@��F@�|�@�;d@���@��@���@�v�@��-@�&�@��/@���@��@�(�@�dZ@��R@�-@�@�@��7@�x�@�&�@��D@��@��@���@��w@��@�;d@�o@��@���@��@���@�z�@�Q�@��@�ƨ@�l�@��H@�M�@���@���@�`B@�/@��@��j@�1'@��F@���@���@��P@�t�@�K�@�33@�o@���@�n�@�E�@�$�@���@��h@�O�@�G�@�7L@�7L@�&�@��@�%@���@��9@��u@�j@�A�@�b@|�@�@~�R@~��@~v�@~V@}�@}�-@}�@}/@|�j@|Z@{��@{��@{33@zM�@y�@y7L@y%@xr�@w�@wK�@v�@v�R@v��@vE�@vE�@v5?@v$�@v@u��@u?}@uV@t�@t�j@tz�@t1@s�F@sS�@r��@q��@q7L@q%@p�`@pĜ@p�9@p��@p1'@n�y@nȴ@n�R@nV@m�@l�@lI�@l�@k�m@kƨ@k�F@k��@k��@kt�@j�!@i�7@i&�@i�@h��@hb@gK�@g�@f��@f{@e@e��@d��@d��@d�@dj@d(�@d1@c��@c��@co@b�!@b~�@a��@a�@a�7@a7L@`�9@`bN@_�@_;d@^��@^�+@^V@^V@^5?@^5?@^5?@^$�@]�T@]�-@]/@\�@\�/@\��@\��@\j@[t�@Z�H@Z��@Z~�@Z�@ZJ@ZJ@ZJ@Y��@Y��@XA�@V��@VE�@V@U�-@U�@UO�@UV@T��@T9X@Sƨ@S�F@S��@St�@SC�@S@R�@R��@R��@R��@R�\@Rn�@Rn�@Rn�@R^5@R-@R-@Q��@Q��@Q7L@P��@PA�@PA�@Pb@O��@O;d@Nȴ@N��@N��@Nv�@NV@NE�@N{@M�h@M?}@L��@Lz�@LI�@L(�@L(�@K��@K�m@Kƨ@K��@K�@KdZ@K"�@J��@J�@I�7@I%@HĜ@HA�@G��@GK�@F��@E�T@E?}@D�D@D1@C�F@CS�@B�@B~�@A��@AX@A�@@�`@@��@@r�@@b@?��@?��@?l�@?
=@>�R@>�R@>��@>{@=@=`B@<��@<�@<�@<�D@;��@;��@;S�@:�@:�!@:-@9�#@9x�@8�9@8Q�@7��@7|�@7\)@7;d@6ȴ@6V@65?@5�@5p�@4�@4�/@4�j@4�@49X@3��@2��@2n�@2^5@2^5@2M�@2-@1��@1�#@1��@1��@1��@1X@1�@0��@0�9@0�@0A�@0 �@0b@0 �@0b@/�;@/|�@.�y@.�y@.�y@.ȴ@.V@-��@-�@-/@,��@,��@,�D@,j@,�@+�m@+�
@+ƨ@+��@+C�@+o@*�H@*��@*^5@*M�@*�@)��@)��@)%@(��@(��@(�@(bN@(A�@(  @'��@'K�@'
=@&��@&�y@&�@&�R@&�R@&��@&�+@&E�@&5?@&{@&@%�T@%��@%��@%@%p�@%/@%�@%V@$��@$�@$�@$�D@$9X@#ƨ@#t�@#33@#@"�H@"��@"�\@"�@!�#@!��@!hs@!G�@!&�@!&�@!�@!%@ �`@ bN@�;@��@�P@|�@l�@l�@�@V@$�@{@@�@@O�@�@�/@��@Z@9X@9X@(�@�@��@��@��@��@�\@n�@^5@=q@��@��@�7@hs@G�@�@�u@r�@A�@�;@�P@+@ff@E�@{@�T@�T@��@�h@��@�/@�/@��@�@z�@�m@�F@S�@C�@"�@�!@=q@�@�^@��@hs@&�@��@��@��@��@�@A�@�@�w@��@�P@�P@�P@\)@�y@��@V@V@E�@{@@�T@�-@/@V@V@V@�/@�@�@�D@�D@�D@�D@�D@��@��@��@��@Z@1@t�@S�@"�@@
�@
�@
��@
M�@	��@	��@	��@	x�@	hs@	7L@	�@��@Ĝ@��@Q�@ �@ �@b@��@l�@
=@�@��@E�@{@�T@@�-@�-@�-@�h@`B@�@�j@�@��@z�@Z@I�@9X@(�@�@1@��@�m@�m@�m@�
@�F@��@��@t�@dZ@S�@33@33@33@o@��@�!@~�@M�@=q@�@��@�#@�7@hs@X@7L@7L@�@%@%@ ��@ Ĝ@ �9@ �u@ �@ r�@ r�@ bN@ A�@ 1'@ b?��w?�\)?��?��R?�V?�{?�{?��?��?��?��-?��h?�O�?�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�`B�B  B%B1BVB{B�B�B�B�B�BVB��B�fB�NB�
B��B�9B��B�7B{�BdZBE�B?}B9XB/B�B�B�BoB
=BB��B�mB�BȴB�^B�'B��B~�Bt�Bm�BgmBdZBaHB^5BXBQ�BI�BA�B9XB(�B�B
��B
�B
�B
�BB
��B
B
�jB
�?B
��B
��B
��B
�bB
~�B
s�B
m�B
k�B
ffB
YB
H�B
6FB
/B
%�B
�B
�B
{B

=B	��B	�sB	�)B	��B	ÖB	�dB	�RB	�3B	�B	��B	��B	��B	��B	�\B	�DB	�+B	�B	�B	{�B	u�B	s�B	p�B	k�B	gmB	bNB	YB	M�B	H�B	B�B	6FB	0!B	/B	/B	,B	'�B	!�B	 �B	�B	�B	�B	�B	oB	1B��B��B�B�B�B�sB�ZB�TB�NB�;B�/B�B�B��B��B��BȴBǮBƨBŢBÖB��B�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�=B�+B�B{�Bs�Bq�Bo�Bn�Bl�BjBgmBgmBffBe`BcTBbNBaHB`BB_;B]/B[#BYBVBQ�BP�BO�BN�BM�BM�BK�BI�BG�BD�BB�BA�B?}B<jB9XB7LB5?B5?B5?B49B33B2-B0!B0!B/B/B.B-B+B)�B(�B'�B%�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�BuBoBoBbBbBbBbB\B\BVBVB\B\B\B\B\B\B\B\B\B\B\BVB\BVBPBVBVBVBVBbBhBhBhBbBbBbBoBuB�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B#�B%�B&�B'�B'�B(�B)�B+B,B.B2-B49B49B49B6FB8RB:^B=qBB�BD�BI�BK�BK�BL�BL�BO�BQ�BZB_;B`BB`BB`BBdZBgmBhsBiyBk�Bk�Bm�Bo�Bo�Br�Bv�Bx�By�By�B{�B|�B|�B~�B� B�B�%B�+B�1B�7B�JB�VB�\B�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�FB�LB�jB�}B��B��BBĜBɺB��B��B��B��B��B��B�B�B�)B�/B�/B�5B�;B�HB�HB�NB�TB�sB�B�B�B�B��B��B��B	B	B	B	+B	1B		7B	DB	hB	�B	�B	�B	�B	�B	 �B	!�B	#�B	&�B	,B	-B	.B	/B	2-B	49B	49B	49B	5?B	5?B	6FB	7LB	8RB	9XB	:^B	;dB	<jB	>wB	A�B	C�B	E�B	F�B	G�B	H�B	J�B	K�B	M�B	O�B	R�B	T�B	XB	YB	\)B	aHB	cTB	ffB	ffB	gmB	hsB	iyB	k�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	p�B	q�B	r�B	r�B	s�B	t�B	u�B	v�B	x�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�\B	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�FB	�XB	�^B	�^B	�qB	�qB	�wB	�}B	��B	��B	��B	B	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�TB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
1B

=B
DB
PB
VB
\B
bB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
#�B
#�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
,B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
XB
YB
YB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B  B%B1BVB�B�B�B�BOB B�B�B��B��BܒB҉B��B��B��B��Bj�BIBA�B<6B2�B BBYB�B�B9B�B�BچB�DB��B�TB�B��Bu�Bn�Bh
Bd�Ba�B_;BYBS[BKxBB�B;�B+QB;B
�}B
��B
�cB
�NB
ҽB
ÖB
��B
��B
��B
�WB
��B
��B
��B
t�B
nB
l�B
h�B
[�B
KxB
7�B
0�B
'B
 �B
�B
�B
B	�B	�B	ޞB	�:B	�B	�PB	�rB	�TB	��B	�eB	�zB	�OB	�B	�bB	�0B	��B	�?B	�[B	}qB	v�B	t�B	q�B	l�B	h�B	d�B	[qB	N�B	JXB	EB	7�B	0�B	/iB	/�B	-B	)DB	"B	!B	;B	B	�B	QB	B	
XB	B�lB��B�cB�B�DB��B�B��B��B��B�WB׍BЗB̈́B�B�RB�1B�B�?B�B�B��B��B��B��B��B��B��B��B��B��B�/B�	B�#B��B�
B��B��B�B��B��B��B�xB�7B��BHBuBrBpoBo�Bm�Bk�Bg�Bg�BgBfBc�Bb�Ba�B`�B_�B^jB\CBZ�BW?BSBQNBPHBO(BNpBNpBL�BK�BIBE�BCaBB[B@�B>]B;dB8B5�B5tB5�B5%B49B2�B0�B0�B/�B/iB.}B./B,B*�B)�B)DB'�B$ZB"�B!�B �B�B�BBKBB�B�B_B�B�B�B�B[BB�BB�B�BHB�BvB�B�B�B�B�BvBvB\BvB�B�B�B�B�B�BBBB(B�BB�B�B�B�BhB�B�B�B�B�BkB#B)B/BB�B�B�BOB!B!B �B!B!bB"�B$�B&LB'RB(XB(sB)_B*B+�B,�B/B2�B4�B4�B4�B7B9>B;B>�BCGBESBJ#BK�BLBM6BMjBP�BS@BZ�B_VB`vB`�B`�Bd�Bg�Bh�Bi�Bk�Bk�Bm�Bo�Bp!BsMBwBy	BzBz*B|B}"B}<BcB��B�{B�?B�zB��B��B��B��B��B��B�B�
B��B��B��B��B��B��B�B��B�&B��B�B�8B�sB�kB�IB�iB�[B�hB�tB��B��B��B��B��B��B��B�9B�=B�6B�B�B�&B�,B�aBևB�eB�]B�IB�dB�jB�pB�|B�bB�B��B�B��B��B��B�B�B�RB�PB	UB	MB	SB	_B	KB		�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$B	'8B	,B	-CB	./B	/OB	2aB	4TB	4TB	4TB	5ZB	5ZB	6`B	7�B	8RB	9rB	:�B	;�B	<�B	>�B	A�B	C�B	E�B	F�B	G�B	H�B	J�B	K�B	NB	PB	S&B	U2B	XEB	YeB	\xB	a|B	c�B	f�B	f�B	g�B	h�B	i�B	k�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	p�B	q�B	r�B	r�B	s�B	t�B	u�B	v�B	y$B	{0B	.B	�'B	�-B	�-B	�3B	�3B	�mB	��B	�\B	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�@B	�B	�B	�=B	�IB	�oB	�MB	�nB	�`B	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	ȴB	��B	��B	��B	�B	�"B	�B	��B	�&B	�B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�1B	�B	�B	�QB	چB	�IB	�OB	�OB	�VB	�BB	�BB	�\B	�\B	�\B	��B	�B	�hB	�NB	�hB	�hB	�NB	�hB	�B	�B	�B	�fB	�B	�fB	�B	�sB	�B	�B	�yB	�_B	�B	�B	�B	�B	�B	�B	�kB	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�BB
 4B
;B
-B
GB
3B
YB
zB
�B

XB
xB
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
$B
$&B
&B
'B
(
B
(
B
(
B
($B
)B
*B
*B
+6B
,=B
,�B
-)B
-)B
-CB
.IB
.IB
/5B
0B
0!B
0!B
0;B
0;B
0;B
1AB
1'B
1AB
1AB
2GB
2GB
2GB
3MB
3MB
3MB
49B
49B
4TB
4TB
4nB
5tB
6FB
6FB
6FB
6zB
6`B
7fB
8lB
8RB
9rB
9rB
9rB
:xB
:xB
:^B
:xB
:xB
;B
;B
<jB
<�B
<�B
=qB
=�B
=�B
=�B
=�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
NB
N"B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
Q�B
RB
RB
Q�B
R�B
SB
R�B
S&B
S@B
T�B
T�B
UB
T�B
UB
U2B
VB
VB
VB
VB
VB
W$B
X+B
X+B
XEB
Y1B
YKB
ZQB
[#B
[=B
\)B
\B
\CB
\CB
\CB
]/B
]/B
]/B
]IB
^OB
^jB
^OB
^OB
_VB
_VB
_pB
`\B
`\B
aHB
aHB
abB
abB
bhB
bhB
bNB
bhB
bhB
bhB
cnB
cnB
cnB
dZB
dZB
d@B
dZB
d�B
dtB
ezB
e`B
e`B
e`B
f�B
f�B
f�B
f�B
gmB
gRB
gmB
g�B
h�B
hsB
hsB
hsB
hsB
hsB
hXB
hXB
hXB
hXB
hsB
h�B
i�B
i�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
|B
|B
|B
|B
|�B
|�B
|�B
|�B
|�B
}B
|�B
}B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*d�<:�<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201907030032322019070300323220190703003232202211182139262022111821392620221118213926201907040016302019070400163020190704001630  JA  ARFMdecpA19c                                                                20190623003731  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190622153733  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190622153735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190622153736  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190622153736  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190622153736  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190622153736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190622153736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190622153737  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190622153737                      G�O�G�O�G�O�                JA  ARUP                                                                        20190622155551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190622153253  CV  JULD            G�O�G�O�F�=                JM  ARCAJMQC2.0                                                                 20190702153232  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190702153232  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190703151630  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123926  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                