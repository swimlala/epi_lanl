CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-28T15:48:30Z creation;2022-09-28T15:48:33Z conversion to V3.1      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
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
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220928154830  20221020131501  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @��X�%�1   @���d��@3�G�z��d�$�/1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A��A   A@  A`  A�  A�  A�  A���A�33A�33A�  A�  B   B  BffB  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Dsy�Dt  Dt� Du  Du� Dv  Dv� Dv��Dwy�Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ DŃ3D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D���D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�D�|�Dּ�D�  D�C3D׃3D�� D�  D�@ D؀ Dؼ�D�  D�@ Dك3D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�C3D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�3D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�{A��A'
=AG
=Ag
=A��A��A��A��A¸RAҸRA�A�BB	B(�BB!B)B1B9\)BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B�{B��HB��HB��HB��HB��HB��HC p�Cp�C�>Cp�Cp�C
�>C�>Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLW
CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�+�C�8RC�8RC�+�C�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�+�C�+�C�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D"�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D"�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D'�D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.��D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4�D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;�D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@�D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da��Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df�Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds�Ds��Dt)Dt�)Du)Du�)Dv)Dv�)Dw�Dw��Dx�Dx�)Dy)Dy�)Dz)Dz�)D{)D{�)D|)D|�)D})D}�)D~)D~�)D)D�)D�D�QGD��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�J�D���D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��GD��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�
�D�J�D��D��D�D�J�D��D��D�D�QGD��D��D�D�J�D��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��GD��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�
�D�J�D��D��D�D�ND��D��D�D�ND��D��D�GD�QGD��D��D�D�ND��GD��GD�GD�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��GD�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D���D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�NDD��D�D�NDÎD��D�D�NDĎD��D�D�NDőGD��D�D�NDƎD��D�D�NDǎD��D�D�NDȎD��D�D�QGDɎD��D�D�NDʎD��D�D�NDˎD��D�D�ND̎D��D�
�D�ND͎D��D�D�NDΎD��D�D�NDώD��D�D�NDЎD��D�D�NDюD��D�D�NDҎD��D�D�NDӎD��D�D�NDԎD��D�D�NDՎD��D�D�J�D֊�D���D�D�QGDבGD��D�D�ND؎D���D�D�NDّGD��D�D�QGDڎD��D�D�NDێD��D�D�QGDܑGD��D�D�NDݎD��D�D�NDގD��D�D�NDߎD��D�D�ND��D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�GD�QGD�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�QGD�D��D�D�ND�D��D�D�ND�GD��D�D�ND�GD��D�
�D�ND��D��D�D�ND�D��D�D�ND�D��D�
�D�ND�D��D�D�QGD�D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�GD�QG11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�I�A�7LA�5?A�E�A�C�A�O�A�I�A�oAܙ�A�oA�VA�{A�^5A�hsA��mA���A���A���A���A�l�A���A�JA��
A��AA�JA�33A�33A��A��A�?}A��\A��A���A�  A�n�A���A�E�A���A���A�/A���A�1A��A�S�A��mA�l�A��A���A�t�A��uA���A��HA�`BA��jA�1'A�ZA���A��A�^5A��A��hA���A�z�A�v�A���A�A�A���A�`BA�1A��^A��A��wA��;A�
=A�;dA�K�A��jA���A���A��#A�Q�A���A���A�jA��A�hsA�\)A�z�A��A��/A�^5A�I�A��A��mA��A��/A�JA��jA��TA���A��A~1'A|E�A{?}AxZAuK�AsdZAq�#Ao�TAmK�Aj(�Ag��Ae�
AdQ�A_ƨA\n�AYƨAV5?AS%AQ33AOt�AMt�AK&�AIS�AI�AHbNAG�mAF��AD��AC�ABbA@{A?VA;��A8�HA6�+A5|�A3�;A1�mA.�A+ƨA)��A(�9A'��A'��A'�A%`BA%VA$�\A$5?A!�A��A ffA 5?A�-A�#A ȴA!�wA ȴAz�A��AK�A�A�!A"�A��Ap�AdZA{A�A|�A1A�A��A  A��A�jA-A��A
=A&�Ax�A�
A�DA
�9A	��A	hsA�A��A��AĜA	"�A�AȴA$�A|�AVA��A&�A��A1'A�AZA-AJAdZA ��A $�@��P@�x�@�Ĝ@���@�Z@�dZ@�~�@�hs@�33@��@�o@��@�7@�G�@�G�@�ƨ@��m@�w@���@�%@�V@�@��m@߮@�b@�(�@߮@���@���@��@�Z@�^5@�O�@�t�@ڗ�@ڏ\@���@��T@�@��@ش9@�1'@׮@��@�(�@�|�@��`@�~�@�V@�G�@�1'@ϝ�@���@̓u@�Q�@ˮ@�^5@�5?@ȓu@ǥ�@�|�@���@�@��T@��`@�@°!@�~�@���@�@���@��@���@���@��D@��m@��@���@���@�l�@���@��@��9@��@��m@���@�@��@�J@��7@��/@�j@�1@�C�@�Z@��;@�z�@���@�~�@���@�E�@�-@�E�@�@���@��;@��@��!@���@��#@��@��@�hs@��j@��D@��@���@�33@��@���@��\@�=q@��h@�7L@�V@��@�b@��m@�"�@��@��T@���@���@�/@���@���@�1@��@��F@�dZ@���@�@���@�r�@���@�t�@�t�@�t�@�t�@�l�@�dZ@�K�@�
=@�~�@�$�@���@���@��7@�x�@�p�@�G�@�V@���@��9@��@�1'@��@�ƨ@��F@���@��@�33@�o@�@��!@�n�@�5?@�@���@�/@���@���@���@�z�@�Z@� �@��w@���@��@�33@��!@�~�@�-@��@��^@��^@�O�@���@�bN@�9X@� �@��@��
@�t�@��@���@���@��\@�n�@�n�@�ff@�^5@�ff@�n�@�^5@�@�{@��@�J@���@�hs@�?}@�&�@��9@�bN@�Q�@�I�@�  @�|�@��@�v�@�=q@���@��T@��T@�`B@�V@��j@���@��D@�I�@���@�\)@��@��@���@�~�@�-@��#@���@���@���@���@�p�@�X@�?}@�7L@��@��/@���@���@��/@��/@���@���@��j@�bN@��@��
@���@�K�@��@��H@��R@��R@�^5@�-@�{@���@��^@���@���@�`B@�&�@�V@�Ĝ@���@�r�@�I�@�b@�  @��@��;@��F@�C�@�
=@��H@��@���@��+@�~�@�$�@��7@�X@�/@�V@�%@���@�V@��@��@�%@��@��@�Q�@�  @�b@��@���@���@�\)@�"�@��\@�M�@�=q@�5?@�$�@��@���@���@�x�@�hs@�`B@�O�@�%@��j@��9@��@���@���@��D@�bN@�9X@�1@
=@~��@~@}��@}`B@|�@|(�@{��@{@z-@y�7@yX@y&�@y�@x��@xA�@w�@w\)@v�y@vE�@v@u�-@uO�@u/@t9X@s�F@so@r��@rn�@r=q@q�@q��@q7L@p�@p �@o�P@o;d@n��@n�@m�-@mO�@m�@l��@l�@k��@kt�@k@j�!@jn�@j-@i�#@i��@h�`@hA�@h �@g��@g��@g�P@g\)@g+@fȴ@f�+@f$�@e�-@e�-@e��@e`B@e?}@d��@dj@d1@cS�@b�H@b�@a�^@ax�@aG�@a&�@`��@`Q�@_�@_|�@^�+@]�T@]V@[��@[��@[t�@[C�@["�@[o@Z�H@Zn�@ZJ@Yhs@XĜ@XbN@X1'@X �@W�@V�y@V�+@U�@T�j@T�D@T1@S�@SC�@So@R��@R=q@Q�@Q�^@QG�@Q&�@Q�@Q%@P�`@P��@P�u@PbN@O�@O�@O\)@N�@NE�@M�T@M��@M��@Mp�@M�@L��@L��@LZ@L�@Kt�@J�@J��@I��@IX@I%@H��@H�@Hb@G�;@G��@G�w@G�@G|�@F�@FE�@E��@EO�@E/@EV@D��@Dz�@D(�@Cƨ@CdZ@B�@B~�@B�@A�#@Ahs@A�@A�@A�@A�@A�@@��@@��@@��@@  @?�P@?|�@?\)@?+@>�y@>��@>V@>{@=@=p�@=p�@=p�@=O�@=/@<�@<��@<�@;�
@;��@;S�@;33@:��@:-@9�^@9x�@9G�@9&�@9%@8Ĝ@8��@8r�@8b@8  @7�@7�;@7�@7��@7��@7��@7�P@7|�@7l�@7+@6�R@6ff@6ff@6E�@6$�@5��@4��@4��@4�@4j@4Z@4I�@4(�@3�F@3��@3��@3C�@3o@2�@2�H@2�H@2~�@2-@1��@1&�@0��@0�u@01'@0b@0  @/�@/;d@/�@.��@.��@.$�@-@-`B@-V@,��@,�@,z�@,j@,Z@,Z@,9X@,1@+�m@+��@+"�@*�!@*n�@*^5@*=q@*�@)��@)��@)7L@(�u@(A�@'��@'l�@'
=@&��@&�+@&ff@%�@%��@%p�@%O�@%?}@%�@%V@$�/@$I�@#S�@"�@"~�@"=q@!�^@!��@!�7@!�7@!x�@!G�@!�@ �`@ Q�@�@�@�P@|�@\)@;d@�@
=@
=@��@V@5?@5?@5?@@�@@?}@��@��@�D@Z@Z@I�@9X@1@�
@�F@S�@C�@33@@�H@��@M�@��@�@��@�7@hs@��@�`@Ĝ@bN@ �@�;@�w@��@l�@\)@;d@�@�+@E�@$�@{@@�-@`B@�@�@z�@Z@(�@��@ƨ@�F@�F@��@dZ@"�@�H@��@~�@^5@-@��@�#@��@��@&�@�`@Ĝ@�9@�9@�9@��@bN@��@;d@
=@��@�y@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@��@$�@�-@�@`B@`B@O�@O�@?}@�@V@�j@��@��@z�@Z@I�@9X@(�@�@�@1@�F@��@��@t�@dZ@dZ@o@
�\@
^5@
=q@
-@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�I�A�7LA�5?A�E�A�C�A�O�A�I�A�oAܙ�A�oA�VA�{A�^5A�hsA��mA���A���A���A���A�l�A���A�JA��
A��AA�JA�33A�33A��A��A�?}A��\A��A���A�  A�n�A���A�E�A���A���A�/A���A�1A��A�S�A��mA�l�A��A���A�t�A��uA���A��HA�`BA��jA�1'A�ZA���A��A�^5A��A��hA���A�z�A�v�A���A�A�A���A�`BA�1A��^A��A��wA��;A�
=A�;dA�K�A��jA���A���A��#A�Q�A���A���A�jA��A�hsA�\)A�z�A��A��/A�^5A�I�A��A��mA��A��/A�JA��jA��TA���A��A~1'A|E�A{?}AxZAuK�AsdZAq�#Ao�TAmK�Aj(�Ag��Ae�
AdQ�A_ƨA\n�AYƨAV5?AS%AQ33AOt�AMt�AK&�AIS�AI�AHbNAG�mAF��AD��AC�ABbA@{A?VA;��A8�HA6�+A5|�A3�;A1�mA.�A+ƨA)��A(�9A'��A'��A'�A%`BA%VA$�\A$5?A!�A��A ffA 5?A�-A�#A ȴA!�wA ȴAz�A��AK�A�A�!A"�A��Ap�AdZA{A�A|�A1A�A��A  A��A�jA-A��A
=A&�Ax�A�
A�DA
�9A	��A	hsA�A��A��AĜA	"�A�AȴA$�A|�AVA��A&�A��A1'A�AZA-AJAdZA ��A $�@��P@�x�@�Ĝ@���@�Z@�dZ@�~�@�hs@�33@��@�o@��@�7@�G�@�G�@�ƨ@��m@�w@���@�%@�V@�@��m@߮@�b@�(�@߮@���@���@��@�Z@�^5@�O�@�t�@ڗ�@ڏ\@���@��T@�@��@ش9@�1'@׮@��@�(�@�|�@��`@�~�@�V@�G�@�1'@ϝ�@���@̓u@�Q�@ˮ@�^5@�5?@ȓu@ǥ�@�|�@���@�@��T@��`@�@°!@�~�@���@�@���@��@���@���@��D@��m@��@���@���@�l�@���@��@��9@��@��m@���@�@��@�J@��7@��/@�j@�1@�C�@�Z@��;@�z�@���@�~�@���@�E�@�-@�E�@�@���@��;@��@��!@���@��#@��@��@�hs@��j@��D@��@���@�33@��@���@��\@�=q@��h@�7L@�V@��@�b@��m@�"�@��@��T@���@���@�/@���@���@�1@��@��F@�dZ@���@�@���@�r�@���@�t�@�t�@�t�@�t�@�l�@�dZ@�K�@�
=@�~�@�$�@���@���@��7@�x�@�p�@�G�@�V@���@��9@��@�1'@��@�ƨ@��F@���@��@�33@�o@�@��!@�n�@�5?@�@���@�/@���@���@���@�z�@�Z@� �@��w@���@��@�33@��!@�~�@�-@��@��^@��^@�O�@���@�bN@�9X@� �@��@��
@�t�@��@���@���@��\@�n�@�n�@�ff@�^5@�ff@�n�@�^5@�@�{@��@�J@���@�hs@�?}@�&�@��9@�bN@�Q�@�I�@�  @�|�@��@�v�@�=q@���@��T@��T@�`B@�V@��j@���@��D@�I�@���@�\)@��@��@���@�~�@�-@��#@���@���@���@���@�p�@�X@�?}@�7L@��@��/@���@���@��/@��/@���@���@��j@�bN@��@��
@���@�K�@��@��H@��R@��R@�^5@�-@�{@���@��^@���@���@�`B@�&�@�V@�Ĝ@���@�r�@�I�@�b@�  @��@��;@��F@�C�@�
=@��H@��@���@��+@�~�@�$�@��7@�X@�/@�V@�%@���@�V@��@��@�%@��@��@�Q�@�  @�b@��@���@���@�\)@�"�@��\@�M�@�=q@�5?@�$�@��@���@���@�x�@�hs@�`B@�O�@�%@��j@��9@��@���@���@��D@�bN@�9X@�1@
=@~��@~@}��@}`B@|�@|(�@{��@{@z-@y�7@yX@y&�@y�@x��@xA�@w�@w\)@v�y@vE�@v@u�-@uO�@u/@t9X@s�F@so@r��@rn�@r=q@q�@q��@q7L@p�@p �@o�P@o;d@n��@n�@m�-@mO�@m�@l��@l�@k��@kt�@k@j�!@jn�@j-@i�#@i��@h�`@hA�@h �@g��@g��@g�P@g\)@g+@fȴ@f�+@f$�@e�-@e�-@e��@e`B@e?}@d��@dj@d1@cS�@b�H@b�@a�^@ax�@aG�@a&�@`��@`Q�@_�@_|�@^�+@]�T@]V@[��@[��@[t�@[C�@["�@[o@Z�H@Zn�@ZJ@Yhs@XĜ@XbN@X1'@X �@W�@V�y@V�+@U�@T�j@T�D@T1@S�@SC�@So@R��@R=q@Q�@Q�^@QG�@Q&�@Q�@Q%@P�`@P��@P�u@PbN@O�@O�@O\)@N�@NE�@M�T@M��@M��@Mp�@M�@L��@L��@LZ@L�@Kt�@J�@J��@I��@IX@I%@H��@H�@Hb@G�;@G��@G�w@G�@G|�@F�@FE�@E��@EO�@E/@EV@D��@Dz�@D(�@Cƨ@CdZ@B�@B~�@B�@A�#@Ahs@A�@A�@A�@A�@A�@@��@@��@@��@@  @?�P@?|�@?\)@?+@>�y@>��@>V@>{@=@=p�@=p�@=p�@=O�@=/@<�@<��@<�@;�
@;��@;S�@;33@:��@:-@9�^@9x�@9G�@9&�@9%@8Ĝ@8��@8r�@8b@8  @7�@7�;@7�@7��@7��@7��@7�P@7|�@7l�@7+@6�R@6ff@6ff@6E�@6$�@5��@4��@4��@4�@4j@4Z@4I�@4(�@3�F@3��@3��@3C�@3o@2�@2�H@2�H@2~�@2-@1��@1&�@0��@0�u@01'@0b@0  @/�@/;d@/�@.��@.��@.$�@-@-`B@-V@,��@,�@,z�@,j@,Z@,Z@,9X@,1@+�m@+��@+"�@*�!@*n�@*^5@*=q@*�@)��@)��@)7L@(�u@(A�@'��@'l�@'
=@&��@&�+@&ff@%�@%��@%p�@%O�@%?}@%�@%V@$�/@$I�@#S�@"�@"~�@"=q@!�^@!��@!�7@!�7@!x�@!G�@!�@ �`@ Q�@�@�@�P@|�@\)@;d@�@
=@
=@��@V@5?@5?@5?@@�@@?}@��@��@�D@Z@Z@I�@9X@1@�
@�F@S�@C�@33@@�H@��@M�@��@�@��@�7@hs@��@�`@Ĝ@bN@ �@�;@�w@��@l�@\)@;d@�@�+@E�@$�@{@@�-@`B@�@�@z�@Z@(�@��@ƨ@�F@�F@��@dZ@"�@�H@��@~�@^5@-@��@�#@��@��@&�@�`@Ĝ@�9@�9@�9@��@bN@��@;d@
=@��@�y@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@��@$�@�-@�@`B@`B@O�@O�@?}@�@V@�j@��@��@z�@Z@I�@9X@(�@�@�@1@�F@��@��@t�@dZ@dZ@o@
�\@
^5@
=q@
-@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B%�BdZB|�B�B�yB+B�B{B�BoB�B��B��BŢB�B�hB�PB�VB�DB�Bs�B_;BXBYBdZBm�Bx�B�B�{B�B�qB��B��B�B�B�B�B�mB�HB�B��B��B��BǮB�3B�hBp�B]/B<jB.B)�B'�B)�B#�B�B"�B�B�B!�B/B.B(�B{BPB��B�B��BÖB�FB��B�bBl�BP�B?}B9XB1'B"�B�B�BoBB
��B
�sB
�;B
�B
��B
��B
B
�3B
�B
��B
��B
�hB
~�B
gmB
YB
J�B
?}B
49B
�B
oB
	7B
  B	�B	�#B	ȴB	�XB	�B	�uB	~�B	o�B	^5B	H�B	>wB	7LB	.B	%�B	�B	 �B	 �B	 �B	�B	�B	hB		7B	+B	B��B�`B�ZB�HB�BB�;B�/B�
B��B��B��B��B�B�B�B�B�BǮB�jBȴB��B��B�B�B	B	B�sB�B�B	B	B��B�`B��B�
B�B�B	B	�B	�B	&�B	49B	:^B	<jB	:^B	-B	!�B	{B	1B��B��B�B�fB�yB�B�B�B�B��B	B	PB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	oB	bB	hB	{B	�B	�B	�B	�B	VB	uB	�B	�B	.B	6FB	5?B	-B	 �B	�B	�B	\B		7B	
=B	\B	�B	�B	 �B	)�B	+B	-B	/B	?}B	?}B	5?B	8RB	=qB	A�B	E�B	K�B	O�B	O�B	Q�B	Q�B	P�B	L�B	5?B	=qB	O�B	R�B	P�B	M�B	M�B	L�B	I�B	J�B	I�B	K�B	S�B	VB	T�B	[#B	`BB	_;B	bNB	dZB	]/B	bNB	bNB	_;B	W
B	YB	XB	ZB	^5B	^5B	^5B	]/B	_;B	dZB	k�B	w�B	{�B	|�B	|�B	|�B	�B	�DB	�PB	�\B	�hB	�hB	�hB	�oB	�hB	�%B	�B	�+B	�uB	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�3B	�-B	�-B	�RB	�jB	�wB	��B	B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	ƨB	ÖB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�ZB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
%B
+B
+B
	7B
	7B

=B
DB
PB
\B
bB
hB
uB
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
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
!�B
$�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
0!B
1'B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
=qB
<jB
;dB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
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
I�B
I�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
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
W
B
XB
W
B
W
B
W
B
XB
XB
XB
W
B
XB
[#B
ZB
[#B
ZB
ZB
ZB
YB
YB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
[#B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
jB
jB
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
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
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
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B�B�B�B�B�B�B�B�B%�BdZB|�B�B�yB+B�B{B�BoB�B��B��BŢB�B�hB�PB�VB�DB�Bs�B_;BXBYBdZBm�Bx�B�B�{B�B�qB��B��B�B�B�B�B�mB�HB�B��B��B��BǮB�3B�hBp�B]/B<jB.B)�B'�B)�B#�B�B"�B�B�B!�B/B.B(�B{BPB��B�B��BÖB�FB��B�bBl�BP�B?}B9XB1'B"�B�B�BoBB
��B
�sB
�;B
�B
��B
��B
B
�3B
�B
��B
��B
�hB
~�B
gmB
YB
J�B
?}B
49B
�B
oB
	7B
  B	�B	�#B	ȴB	�XB	�B	�uB	~�B	o�B	^5B	H�B	>wB	7LB	.B	%�B	�B	 �B	 �B	 �B	�B	�B	hB		7B	+B	B��B�`B�ZB�HB�BB�;B�/B�
B��B��B��B��B�B�B�B�B�BǮB�jBȴB��B��B�B�B	B	B�sB�B�B	B	B��B�`B��B�
B�B�B	B	�B	�B	&�B	49B	:^B	<jB	:^B	-B	!�B	{B	1B��B��B�B�fB�yB�B�B�B�B��B	B	PB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	oB	bB	hB	{B	�B	�B	�B	�B	VB	uB	�B	�B	.B	6FB	5?B	-B	 �B	�B	�B	\B		7B	
=B	\B	�B	�B	 �B	)�B	+B	-B	/B	?}B	?}B	5?B	8RB	=qB	A�B	E�B	K�B	O�B	O�B	Q�B	Q�B	P�B	L�B	5?B	=qB	O�B	R�B	P�B	M�B	M�B	L�B	I�B	J�B	I�B	K�B	S�B	VB	T�B	[#B	`BB	_;B	bNB	dZB	]/B	bNB	bNB	_;B	W
B	YB	XB	ZB	^5B	^5B	^5B	]/B	_;B	dZB	k�B	w�B	{�B	|�B	|�B	|�B	�B	�DB	�PB	�\B	�hB	�hB	�hB	�oB	�hB	�%B	�B	�+B	�uB	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�3B	�-B	�-B	�RB	�jB	�wB	��B	B	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	ƨB	ÖB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�ZB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
%B
+B
+B
	7B
	7B

=B
DB
PB
\B
bB
hB
uB
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
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
!�B
$�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
0!B
1'B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
=qB
<jB
;dB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
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
I�B
I�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
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
W
B
XB
W
B
W
B
W
B
XB
XB
XB
W
B
XB
[#B
ZB
[#B
ZB
ZB
ZB
YB
YB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
[#B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
jB
jB
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
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
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
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220929004755  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220928154830  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220928154831  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220928154833                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220929004837  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220929004837  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220928160237                      G�O�G�O�G�O�                JA  COOAcooa1.0                                                                 20221020115553  CF  PSAL            B   D�C3?�                  JA  COOAcooa1.0                                                                 20221020115553  CF  PSAL            @���A�  ?�                  JA  COOAcooa1.0                                                                 20221020115553  CF  PSAL_ADJUSTED   @���B0  ?�                  JA  COOAcooa1.0                                                                 20221020115553  CF  PSAL_ADJUSTED   B7��D�C3?�                  JA  ARUP                                                                        20221020131501                      G�O�G�O�G�O�                