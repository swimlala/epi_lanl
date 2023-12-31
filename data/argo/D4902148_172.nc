CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-05-03T15:36:46Z creation;2019-05-03T15:36:51Z conversion to V3.1;2019-12-18T07:15:20Z update;2022-11-21T05:29:00Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20190503153646  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_172                     2C  Dd(�NAVIS_A                         0397                            ARGO 011514                     863 @ػ��� 1   @ػ b� @;��'��d(��}Vm1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C5��C7��C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C���C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�=DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D�D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111oA�A�%A�VA�bA�VA�VA�bA�JA�VA�VA�%A�  A��A��uA�7LA�K�A���A�VA���A���A���A�n�A�n�A�E�A�?}A�$�A�p�A���A�XA���A�x�A��
A�oA��jA�S�A��PA�?}A��A�oA�%A��
A�ZA�/A��A�ffA�VA���A�ƨA��-A�`BA�"�A���A�S�A��A���A�l�A�ffA�"�A���A�C�A��
A�?}A�bA��;A�?}A�-A���A��A��\A���A�x�A�\)A�7LA��wA���A�+A��!A���A��^A�oA~(�A|1Ay��Aw�wAv�yAu?}At��AtE�As�7ArM�Aq�FAq;dApz�Aop�An��Am/Ak�Aj��Ah�Af�/Ad�jAcS�Ab��Ab�\A`�/A^��A]"�A\��A\�A[t�AZ�AZn�AY�AWS�AT��AT{AS�PAR�AQ�AQ��AQVAN�DAL�AJ��AJr�AJ=qAI�AIƨAI?}AHĜAHbNAG��AG|�AGoAFĜAFz�AFA�AE�AEt�AD��AB��A@�!A?��A?O�A>��A=�A=��A=p�A<n�A;��A;?}A9��A8��A7�A7K�A6��A6��A6��A6�\A6ffA5�A5��A5l�A5;dA4��A4��A3��A2�`A2n�A1x�A0�!A0(�A/S�A.r�A-�A,ĜA,=qA+�hA+/A*^5A)|�A(��A(��A(�A(M�A(bA'��A'C�A%/A#;dA ��A��A7LA%A��AI�A�A��A�A"�A�A+A%Ar�A��A;dAjA��A;dA�A�DAA�A��A`BA%A�`A�DA-A|�A/A�!A�+A-AhsAJA�mA��A|�AdZA`BAO�A?}A7LA"�A
��A
�A
ĜA	7LA/A�\A�A��A�A��AC�A ��A ��@��w@���@�5?@���@��@��@�?}@�1@�
=@�{@�9@�ƨ@�+@�"�@�"�@�o@��y@���@�R@�!@�V@�&�@�9X@��@��@ޏ\@���@ܬ@�K�@�@�I�@׾w@���@�hs@�%@ҟ�@љ�@�z�@�A�@� �@� �@�1@��#@˾w@˅@�"�@��y@ʸR@�=q@���@�X@ȣ�@Ǿw@�S�@�
=@�@ļj@��@öF@Ý�@�K�@���@�v�@��@��T@���@�&�@�I�@�t�@���@��\@�^5@�@��^@�X@��u@��m@�l�@���@�ff@��-@�7L@��@�I�@���@���@��@��@�Ĝ@��;@���@��@���@���@�bN@���@��/@��@��@�\)@�+@��@�M�@��@��^@�O�@�r�@� �@�b@�1@�1@�1@�b@���@��
@��y@�G�@���@�Q�@�dZ@��@��@�?}@�9X@�|�@�ȴ@�{@�&�@���@��@���@�;d@��R@�J@���@��w@��@��@��7@���@�j@�1@��F@�S�@�@���@��R@���@���@��+@�v�@�ff@�V@�E�@�-@�{@��@�x�@�%@�Q�@�ȴ@�v�@�V@�5?@���@�@��7@�`B@��@��`@���@���@��D@�r�@�Z@�A�@�1@��w@�|�@���@�n�@�=q@�$�@�$�@�$�@��T@��^@���@��@�`B@�7L@�/@��@�V@��@���@��@�r�@�b@�t�@�
=@�v�@��@�@���@��h@�O�@���@�Ĝ@���@�j@�9X@�;@�P@l�@K�@~ȴ@~$�@}�@}��@}�-@}`B@}�@|�@{�F@z��@y�^@yx�@y7L@y%@x�`@xĜ@x�9@xr�@wK�@vE�@u�-@uO�@t�@t�j@s�m@sdZ@s33@s@r��@r�\@r=q@r�@q�7@pĜ@p�@p  @o�w@o��@o�P@ol�@ol�@oK�@o
=@n@lI�@kt�@j�H@j~�@j-@i�@iG�@hĜ@h�u@hA�@g�;@g�@g|�@g+@g+@g+@g+@f�R@e�@e�-@e�h@ep�@eO�@e�@e�h@e�h@d�/@d�@c�
@c�F@ct�@cS�@c33@c"�@c"�@c"�@co@b�@b��@ahs@`�`@`�9@`r�@`Q�@`1'@` �@_�;@_��@_|�@_
=@^V@]��@\�/@\�D@\I�@[��@[�@[C�@Z��@Zn�@YX@XĜ@X�@X1'@W�P@V�y@V$�@U��@U@U�@T�@T9X@T(�@S��@T1@T1@T1@Sƨ@S�F@Sƨ@Sƨ@S��@S��@S�F@S��@S�@S�@SdZ@R�!@Q��@Q��@Qhs@QX@Q7L@P��@O�@Nv�@NE�@M@M�h@L�j@L�@K��@K��@K��@Kt�@K"�@J�\@J�@I��@I��@I�^@I��@Ix�@IX@IG�@I7L@I&�@I7L@H�@HbN@HA�@H  @G��@G�w@Gl�@F�y@Fff@E@E?}@E/@D�@D�@DZ@D9X@D�@D1@D1@C�m@C��@C�@CC�@C33@Co@B�!@Bn�@Bn�@B-@A�^@A��@Ax�@AX@A�@@��@@�9@?�@?��@?��@?�w@?�w@?��@?�@>��@>��@>ff@=�h@=`B@=p�@=p�@=p�@=p�@=p�@=p�@=`B@<��@<z�@<I�@<�@;�
@;�m@;�@;S�@;dZ@;dZ@;S�@;C�@;33@;"�@;o@:�!@:-@9�#@9��@97L@8�`@8��@8�9@8��@8�u@8b@7��@7+@6��@6ȴ@6��@6V@5�T@5@5��@5O�@4j@3ƨ@3o@2�H@2�\@2J@1��@17L@17L@1�@0�9@0r�@/�;@/l�@/;d@/�@.��@.�@.�R@.�+@.v�@.V@.E�@.@-�@,�@+��@+33@*��@*=q@)��@)��@)�7@)hs@)hs@)G�@)G�@)7L@)�@(��@(��@(�9@(��@(�@(r�@(A�@( �@(  @'�;@'�P@'K�@&��@&�R@&��@&$�@%@%�@%O�@%?}@%/@%V@$�@$��@$�j@$z�@$Z@$9X@$�@$1@$1@$1@$1@#��@#�
@#�
@#�
@#�
@#ƨ@#ƨ@#�F@#ƨ@#��@#��@#�@#t�@#C�@#33@#o@"�H@"��@"��@"��@"n�@"M�@"=q@"-@"�@"J@!��@!�@!��@!��@!�^@!��@!��@!��@!x�@ �9@ �u@   @�;@�P@�P@|�@l�@+@�@�@�@�@�@+@�@+@+@
=@
=@
=@�@��@�@ȴ@ȴ@�R@�R@�R@��@��@��@v�@v�@E�@�@�h@O�@/@/@/@�@�@�@�/@�/@I�@�F@dZ@��@M�@�^@X@&�@��@��@Ĝ@�@Q�@A�@ �@�;@�w@�P@l�@;d@+@�y@v�@ff@E�@5?@$�@@�@@p�@?}@�@(�@�m@��@t�@S�@"�@�@~�@�@��@7L@&�@��@A�@�w@l�@+@��@ȴ@ff@�-@O�@O�@O�@/@/@�@V@(�@�F@dZ@
��@
�!@
��@
n�@	��@	��@	��@	x�@	&�@��@�9@Q�@A�@1'@1'@ �@ �@b@�@��@�P@|�@|�@l�@�y@ff@5?@�@��@O�@?}@V@�@z�@j@Z@I�@(�@�m@ƨ@�@"�@�H@�!@~�@n�@^5@M�@=q@-@J@�@�@�@�@�#@�#@��@�7@hs@G�@G�@7L@ �`@ Ĝ@ 1'?��;?��w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111oA�A�%A�VA�bA�VA�VA�bA�JA�VA�VA�%A�  A��A��uA�7LA�K�A���A�VA���A���A���A�n�A�n�A�E�A�?}A�$�A�p�A���A�XA���A�x�A��
A�oA��jA�S�A��PA�?}A��A�oA�%A��
A�ZA�/A��A�ffA�VA���A�ƨA��-A�`BA�"�A���A�S�A��A���A�l�A�ffA�"�A���A�C�A��
A�?}A�bA��;A�?}A�-A���A��A��\A���A�x�A�\)A�7LA��wA���A�+A��!A���A��^A�oA~(�A|1Ay��Aw�wAv�yAu?}At��AtE�As�7ArM�Aq�FAq;dApz�Aop�An��Am/Ak�Aj��Ah�Af�/Ad�jAcS�Ab��Ab�\A`�/A^��A]"�A\��A\�A[t�AZ�AZn�AY�AWS�AT��AT{AS�PAR�AQ�AQ��AQVAN�DAL�AJ��AJr�AJ=qAI�AIƨAI?}AHĜAHbNAG��AG|�AGoAFĜAFz�AFA�AE�AEt�AD��AB��A@�!A?��A?O�A>��A=�A=��A=p�A<n�A;��A;?}A9��A8��A7�A7K�A6��A6��A6��A6�\A6ffA5�A5��A5l�A5;dA4��A4��A3��A2�`A2n�A1x�A0�!A0(�A/S�A.r�A-�A,ĜA,=qA+�hA+/A*^5A)|�A(��A(��A(�A(M�A(bA'��A'C�A%/A#;dA ��A��A7LA%A��AI�A�A��A�A"�A�A+A%Ar�A��A;dAjA��A;dA�A�DAA�A��A`BA%A�`A�DA-A|�A/A�!A�+A-AhsAJA�mA��A|�AdZA`BAO�A?}A7LA"�A
��A
�A
ĜA	7LA/A�\A�A��A�A��AC�A ��A ��@��w@���@�5?@���@��@��@�?}@�1@�
=@�{@�9@�ƨ@�+@�"�@�"�@�o@��y@���@�R@�!@�V@�&�@�9X@��@��@ޏ\@���@ܬ@�K�@�@�I�@׾w@���@�hs@�%@ҟ�@љ�@�z�@�A�@� �@� �@�1@��#@˾w@˅@�"�@��y@ʸR@�=q@���@�X@ȣ�@Ǿw@�S�@�
=@�@ļj@��@öF@Ý�@�K�@���@�v�@��@��T@���@�&�@�I�@�t�@���@��\@�^5@�@��^@�X@��u@��m@�l�@���@�ff@��-@�7L@��@�I�@���@���@��@��@�Ĝ@��;@���@��@���@���@�bN@���@��/@��@��@�\)@�+@��@�M�@��@��^@�O�@�r�@� �@�b@�1@�1@�1@�b@���@��
@��y@�G�@���@�Q�@�dZ@��@��@�?}@�9X@�|�@�ȴ@�{@�&�@���@��@���@�;d@��R@�J@���@��w@��@��@��7@���@�j@�1@��F@�S�@�@���@��R@���@���@��+@�v�@�ff@�V@�E�@�-@�{@��@�x�@�%@�Q�@�ȴ@�v�@�V@�5?@���@�@��7@�`B@��@��`@���@���@��D@�r�@�Z@�A�@�1@��w@�|�@���@�n�@�=q@�$�@�$�@�$�@��T@��^@���@��@�`B@�7L@�/@��@�V@��@���@��@�r�@�b@�t�@�
=@�v�@��@�@���@��h@�O�@���@�Ĝ@���@�j@�9X@�;@�P@l�@K�@~ȴ@~$�@}�@}��@}�-@}`B@}�@|�@{�F@z��@y�^@yx�@y7L@y%@x�`@xĜ@x�9@xr�@wK�@vE�@u�-@uO�@t�@t�j@s�m@sdZ@s33@s@r��@r�\@r=q@r�@q�7@pĜ@p�@p  @o�w@o��@o�P@ol�@ol�@oK�@o
=@n@lI�@kt�@j�H@j~�@j-@i�@iG�@hĜ@h�u@hA�@g�;@g�@g|�@g+@g+@g+@g+@f�R@e�@e�-@e�h@ep�@eO�@e�@e�h@e�h@d�/@d�@c�
@c�F@ct�@cS�@c33@c"�@c"�@c"�@co@b�@b��@ahs@`�`@`�9@`r�@`Q�@`1'@` �@_�;@_��@_|�@_
=@^V@]��@\�/@\�D@\I�@[��@[�@[C�@Z��@Zn�@YX@XĜ@X�@X1'@W�P@V�y@V$�@U��@U@U�@T�@T9X@T(�@S��@T1@T1@T1@Sƨ@S�F@Sƨ@Sƨ@S��@S��@S�F@S��@S�@S�@SdZ@R�!@Q��@Q��@Qhs@QX@Q7L@P��@O�@Nv�@NE�@M@M�h@L�j@L�@K��@K��@K��@Kt�@K"�@J�\@J�@I��@I��@I�^@I��@Ix�@IX@IG�@I7L@I&�@I7L@H�@HbN@HA�@H  @G��@G�w@Gl�@F�y@Fff@E@E?}@E/@D�@D�@DZ@D9X@D�@D1@D1@C�m@C��@C�@CC�@C33@Co@B�!@Bn�@Bn�@B-@A�^@A��@Ax�@AX@A�@@��@@�9@?�@?��@?��@?�w@?�w@?��@?�@>��@>��@>ff@=�h@=`B@=p�@=p�@=p�@=p�@=p�@=p�@=`B@<��@<z�@<I�@<�@;�
@;�m@;�@;S�@;dZ@;dZ@;S�@;C�@;33@;"�@;o@:�!@:-@9�#@9��@97L@8�`@8��@8�9@8��@8�u@8b@7��@7+@6��@6ȴ@6��@6V@5�T@5@5��@5O�@4j@3ƨ@3o@2�H@2�\@2J@1��@17L@17L@1�@0�9@0r�@/�;@/l�@/;d@/�@.��@.�@.�R@.�+@.v�@.V@.E�@.@-�@,�@+��@+33@*��@*=q@)��@)��@)�7@)hs@)hs@)G�@)G�@)7L@)�@(��@(��@(�9@(��@(�@(r�@(A�@( �@(  @'�;@'�P@'K�@&��@&�R@&��@&$�@%@%�@%O�@%?}@%/@%V@$�@$��@$�j@$z�@$Z@$9X@$�@$1@$1@$1@$1@#��@#�
@#�
@#�
@#�
@#ƨ@#ƨ@#�F@#ƨ@#��@#��@#�@#t�@#C�@#33@#o@"�H@"��@"��@"��@"n�@"M�@"=q@"-@"�@"J@!��@!�@!��@!��@!�^@!��@!��@!��@!x�@ �9@ �u@   @�;@�P@�P@|�@l�@+@�@�@�@�@�@+@�@+@+@
=@
=@
=@�@��@�@ȴ@ȴ@�R@�R@�R@��@��@��@v�@v�@E�@�@�h@O�@/@/@/@�@�@�@�/@�/@I�@�F@dZ@��@M�@�^@X@&�@��@��@Ĝ@�@Q�@A�@ �@�;@�w@�P@l�@;d@+@�y@v�@ff@E�@5?@$�@@�@@p�@?}@�@(�@�m@��@t�@S�@"�@�@~�@�@��@7L@&�@��@A�@�w@l�@+@��@ȴ@ff@�-@O�@O�@O�@/@/@�@V@(�@�F@dZ@
��@
�!@
��@
n�@	��@	��@	��@	x�@	&�@��@�9@Q�@A�@1'@1'@ �@ �@b@�@��@�P@|�@|�@l�@�y@ff@5?@�@��@O�@?}@V@�@z�@j@Z@I�@(�@�m@ƨ@�@"�@�H@�!@~�@n�@^5@M�@=q@-@J@�@�@�@�@�#@�#@��@�7@hs@G�@G�@7L@ �`@ Ĝ@ 1'?��;?��w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B\B\B\B\B\B\B\B\B\B\B\BVBJB%B��B�B�B�sB�NB�/B�
B��BĜBB��B�}B�XB�FB�-B��B��B�JB�B~�B}�B�B}�B}�B~�B}�B|�Bx�Bu�Bq�BjBe`BbNBaHB_;B^5BYBQ�BD�B.BB��B��B��B�B�yB��B�}B�RB��By�BT�BH�BA�B;dB5?B33B#�BVBB
�B
�BB
ŢB
�LB
��B
�%B
u�B
dZB
P�B
B�B
<jB
1'B
-B
(�B
#�B
�B
�B
uB
PB
%B
  B	��B	�B	�ZB	�
B	��B	��B	�RB	�9B	�-B	��B	��B	�uB	�hB	�PB	�7B	�B	�B	|�B	q�B	bNB	_;B	\)B	YB	W
B	T�B	Q�B	G�B	B�B	=qB	:^B	9XB	7LB	6FB	2-B	0!B	.B	+B	(�B	'�B	%�B	$�B	"�B	 �B	�B	�B	oB	+B	B	B��B��B��B��B�B�B�B�fB�NB�BB�;B�5B�/B�)B�)B�#B�#B�B�B�
B�
B�B��B��BɺBB�}B�^B�9B�B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�B}�B{�Bz�Bz�By�Bx�Bw�Bu�Bs�Br�Bo�Bn�Bo�Bn�Bn�Bk�BiyBgmBffBdZBcTBbNBbNBbNB`BB_;B_;B^5B\)B[#BZBYBW
BVBR�BN�BJ�BH�BF�BF�BF�BF�BF�BF�BF�BE�BE�BC�B@�B;dB5?B0!B/B/B.B.B,B+B(�B$�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B"�B#�B$�B$�B%�B%�B%�B.B0!B2-B49B49B49B6FB9XB;dB;dB=qB=qBB�BH�BJ�BL�BL�BM�BN�BP�BQ�BQ�BR�BW
BXBXBYBYBYBYBYBYB\)BbNBcTBffBjBk�Bo�Bs�Bw�Bz�B}�B�B�B�%B�1B�DB�JB�VB�oB��B��B��B��B�B�!B�-B�9B�?B�LB�XB�^B�dB�jB�jB�jB�qB�qB�qB�wB�wB�}B��BÖBŢB��B�B�B�#B�/B�;B�HB�ZB�`B�yB�B�B�B�B�B��B��B��B��B��B	B	+B		7B	
=B	
=B	
=B	PB	VB	bB	hB	oB	{B	{B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	+B	/B	0!B	2-B	2-B	5?B	8RB	9XB	;dB	<jB	>wB	@�B	B�B	B�B	C�B	E�B	H�B	I�B	I�B	J�B	K�B	L�B	L�B	Q�B	VB	YB	ZB	[#B	[#B	\)B	\)B	\)B	]/B	aHB	e`B	gmB	iyB	jB	k�B	n�B	p�B	p�B	q�B	q�B	r�B	t�B	t�B	v�B	y�B	z�B	|�B	}�B	~�B	~�B	� B	� B	� B	�B	�B	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�?B	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�;B	�HB	�TB	�ZB	�ZB	�ZB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B

=B

=B
DB
JB
JB
JB
JB
JB
PB
PB
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
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
,B
.B
.B
.B
/B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
7LB
8RB
9XB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
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
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B\B\B\B\B\B\B\B\B\BvB�B�B�B	�B�B��B��B��B�B�VB��B̈́B�B��B�B�B��B�B�B��B��B��B��B�B}B��B~BB~(B.B~�B}�By>Bv�Br�Bk6Be�BbhBa�B_�B^�BZBS[BF�B0�B�B�B��B��B�5B�WB�\B��B�dB��B}�BV�BJ	BB�B<�B6�B5�B&2B�B9B
��B
�TB
��B
��B
�ZB
��B
xlB
gB
R�B
C�B
>B
1�B
-�B
)�B
%,B
xB
$B
{B
�B
zB
�B	��B	�IB	��B	�1B	�"B	�B	�$B	�%B	�nB	�eB	�/B	�,B	�TB	�B	�	B	��B	��B	�B	tTB	cTB	`'B	]dB	Y�B	W�B	VmB	T�B	JXB	C�B	=�B	:�B	9�B	7�B	6�B	2�B	0�B	.�B	+�B	)yB	(>B	&2B	%FB	#TB	!�B	 'B	�B	�B	fB	�B	�B��B�^B�rB�B�B��B�wB�B�:B��BߤBބB�dB�CBܒBۦBیB�eB�yB׍BרB�$B�2BΊB��BðB�iB��B�tB�IB��B��B��B��B��B��B�OB��B�B�B��B�2B��B� B��B��B}B{B{JBz^ByXBx8BvFBtTBs�BqABoOBpBoiBo�BlWBjBhXBgBd�Bc�Bb�BcBb�B`�B_�B_�B^�B]B[�BZ�BY�BW�BWsBUBQ�BL�BIRBF�BF�BF�BF�BF�BF�BF�BFBF�BF%BCaB>�B7�B0�B/OB/OB.�B.�B,�B,WB*�B&�B#�B!B�B5BWBWBWBkBB�B�B�B�B�B�B�B�B�B�B�B�B�B#BQB�B�B�B�B1BeByBEBB_BEB�B�B�B+B�B�B�B�B�B�B�B�BBBBBBKBQB#B�B�B�B�BB�B�B�B/BjB5B!B�B B 'B!B!-B!bB"4B#TB$ZB%FB%zB&�B'8B(
B.�B0�B2�B4TB4�B5B72B9�B;�B<B>B>�BC�BIBK)BMBMBN"BOBBQBR BRTBS�BW$BXBX+BY1BYBY1BY1BYeBY�B]/Bb�Bc�BgBj�Bl=BpUBtnBxRB{B~�B��B�gB��B��B��B��B�B�[B�=B�VB�zB�KB��B�UB�aB�nB�tB��B��B�xB�B��B�jB�jB��B��B��B�wB��B��B��B��B�YBˬB�EB�7B�=B�dB�pB�|B�tB�zB�B�B��B�B��B�B��B��B�*B�<B�cB	aB	EB		RB	
XB	
=B	
rB	�B	pB	}B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	;B	$@B	&fB	+QB	/5B	0;B	2-B	2aB	5�B	8�B	9rB	;�B	<�B	>�B	@�B	B�B	B�B	C�B	E�B	H�B	I�B	I�B	J�B	K�B	MB	MPB	RTB	V9B	Y1B	Z7B	[=B	[=B	\)B	\)B	\]B	]�B	a�B	ezB	g�B	i�B	j�B	k�B	n�B	p�B	p�B	q�B	q�B	r�B	t�B	t�B	wB	y�B	{B	}B	~B	B	B	�B	�B	�4B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�B	�B	�"B	��B	�'B	�-B	�|B	��B	�XB	�xB	�xB	�^B	�B	�B	�jB	�PB	��B	�jB	��B	��B	��B	ÖB	ÖB	ĶB	ĶB	ŢB	żB	��B	��B	��B	�B	��B	�B	��B	��B	�B	� B	�B	�,B	�B	�mB	�QB	�=B	�dB	ބB	ߊB	�|B	�nB	�tB	�B	�B	�B	�mB	�B	�mB	�RB	�mB	�mB	�mB	�RB	�mB	�B	�mB	�RB	�B	�mB	�B	�B	�B	��B	�B	�B	�B	��B	��B	�5B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B
  B
 B
 B
 B
 B
B
B
 B
 B
;B
B
B
B
-B
3B
MB
SB
YB
_B
	lB
	7B

=B

XB
DB
dB
dB
JB
0B
dB
jB
PB
PB
pB
pB
pB
\B
bB
}B
�B
hB
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
#�B
$B
%B
&B
&�B
&�B
(
B
'�B
($B
(�B
)B
)*B
*KB
+6B
,=B
.B
.IB
.IB
/OB
0;B
1'B
1'B
1AB
1[B
2aB
3hB
3MB
4TB
4TB
4TB
49B
49B
5?B
5ZB
5ZB
5ZB
5�B
7�B
8�B
9�B
:xB
;�B
<�B
=�B
=qB
=qB
=VB
=qB
>wB
>wB
>wB
>wB
>�B
>�B
?}B
?}B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
Q B
Q B
Q�B
P�B
Q�B
RB
Q�B
RB
Q�B
RB
R B
S&B
TFB
UB
U2B
VB
W$B
W$B
W$B
W
B
W$B
XB
XB
X+B
XB
X+B
Y1B
Y1B
YB
YB
Y1B
Y1B
ZB
Z7B
ZB
[#B
[#B
[=B
[#B
[#B
[=B
\]B
\]B
]/B
^OB
^5B
^OB
^OB
_VB
_pB
_pB
`\B
`vB
aHB
a|B
b�B
bhB
cnB
dtB
dtB
dtB
d�B
e�B
ffB
gRB
gRB
g�B
gRB
gmB
g�B
g�B
h�B
i�B
i�B
jB
j�B
jB
j�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
mwB
n�B
n}B
n}B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905140031432019051400314320190514003143202211182138532022111821385320221118213853201905150019532019051500195320190515001953  JA  ARFMdecpA19c                                                                20190504003643  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190503153646  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190503153649  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190503153649  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190503153650  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190503153650  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190503153650  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190503153650  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190503153650  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190503153651                      G�O�G�O�G�O�                JA  ARUP                                                                        20190503155537                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190503153114  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190513153143  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190513153143  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190514151953  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123853  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                