CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-02T09:42:30Z creation;2023-05-02T09:42:34Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20230502094230  20230502095901  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�(
n,��1   @�(q�@4�\(��c�\(�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @���A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DPy�DP��DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dxy�Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~�fDfD�fD�3D�@ D�� D�� D�  D�C3D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؃3D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�3D�C3D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�C3D�3D��3D�  D�@ D� D�� D�  D�C3D�3D��3D�  D�@ D� D��D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@��HA
=A(��AH��Ag
=A��A��A��A��AÅAӅA�A�BB	BBB!B)B1B9BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB�{B�{B��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:W
C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CV�>CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cf�>Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�8RC�EC�8RC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�+�C�8RC�8RC�+�C�+�C�8RC�8RC�8RC�8RC�8RD )D �)D"�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D"�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!"�D!�)D")D"�)D#)D#�)D$)D$��D%"�D%�)D&)D&�)D')D'�)D(�D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.��D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4"�D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;�D;��D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP�DP��DQ�DQ�)DR)DR�)DS)DS�)DT)DT��DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\�D\��D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd�Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl��Dm�Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du)Du�)Dv)Dv�)Dw)Dw�)Dx�Dx��Dy)Dy�)Dz)Dz�)D{)D{�)D|)D|�)D})D}�)D~"�D~��D"�D��D�GD�ND��D��D�D�QGD��GD��GD�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�
�D�ND��D��D�D�QGD��D��D�D�ND��D��D�D�QGD��D���D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�GD�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��GD�GD�ND��D��D�D�ND��D��D�D�ND��GD��D�D�ND��D��GD�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�QGD��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�QGD��D��D�D�ND��D��D�D�ND��D��D�
�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�
�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�
�D�ND��D��D�D�ND��D���D�
�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�NDD��D�D�NDÎD��D�
�D�NDĎD��D�D�NDŎD��D�D�NDƎD��D�D�NDǎD��D�D�NDȎD��D�D�NDɎD��D�D�NDʎD��D�D�NDˎD��D�D�ND̎D��D�D�ND͎D��D�D�NDΎD��D�D�NDώD��D�D�NDЎD��D�D�NDюD��D�D�NDҎD��D�D�NDӎD��D�D�NDԎD��D�D�NDՎD��D�D�ND֎D��D�D�ND׎D��D�D�NDؑGD��D�D�NDَD��D�D�NDڎD��D�D�NDێD��D�D�ND܎D���D�D�NDݎD��D�D�NDގD��D�D�NDߎD��D�D�ND��D��GD�GD�QGD�D��D�D�ND�D��D�D�QGD�GD��D�D�ND�D��D�D�ND�D��D�D�ND�D��D�D�ND�D��GD�GD�QGD�GD��GD�D�ND�D��D�D�QGD�GD��GD�D�ND�D���D�D�ND�D��GD�D�ND�D��D�D�ND�D��D�D�ND�D��D�
�D�J�D��D��D�D�ND�D��D�D�ND�D��D�D�ND��D��D�D�ND�D��D�D�ND��D��D�D�ND��D��D�D�ND��D��D�D�ND��D��GD�GD�QGD��G1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�{A��;AЁA�9XA�$�A�A��mA�Aϟ�AϓuAύPAρA�z�A�x�A�r�A�n�A�n�A�hsA�ffA�dZA�`BAͩ�A�+A�E�A��
A�9XA�E�A�?}A�p�A��Aư!A�~�A�A�{A�A�jA��mA�^5A�A���A�v�A� �A�A�A��A�Q�A�A��A���A�dZA��jA���A�XA���A��DA�dZA�C�A���A��hA��#A��A�ZA��yA�`BA�
=A���A�1'A�dZA��A���A�  A���A��A�1'A��FA��PA��A���A��A�"�A��A�n�A��A��HA�ĜA��^A���A��DA�VA��HA��yA��\A�ZA�hsA��+A�v�A�ZA��-A���A�XA�;dA�oA���A�XA���A�1A� �A��^A��wA��A�  A��A�ĜA���A�&�A}�Ax�At��Aq7LAm33Ak&�Ag/Ac�TA_?}A]t�A\�AZ1'AWAS&�AQp�AQ"�AQ�AQ�AQAP�AK�-AH1AF��AE�FAE\)AD�jAAl�A?XA=
=A:A�A8  A6(�A4�yA3ƨA3oA1�A0A�A//A.{A-?}A,Q�A+C�A)��A)�A(�`A(��A'�A&-A$VA$�A#�mA#��A#`BA"��A"VA!K�A�hA"�A��A\)AG�A�PA�/A��A�+A=qA�mA��A�!A�wA��A�;AJA�A-A�A��A��AXA�A��A
VA�-A
=An�Al�A�AjAVAE�A �A�PAAC�A?}Ap�@�ƨ@�n�@�%@��7@��
@�K�@��H@�ff@��@��@�@�@���@�-@�Q�@�7L@�l�@�-@�7L@�j@�j@�  @���@�v�@�O�@���@ޟ�@���@�%@�=q@��@؃@�1'@�;d@��@��T@��#@���@�ff@��/@�v�@ѡ�@��#@Ѳ-@�G�@��@��@�%@ЋD@��;@���@��#@�G�@̃@��@��@�{@ɑh@�Z@ǝ�@ư!@ļj@�b@���@ÍP@§�@���@���@�bN@���@�ȴ@��+@�E�@���@���@�/@��@��;@��y@���@��^@��/@���@��@�bN@�1@���@��@�K�@�S�@�"�@�
=@��@��\@�~�@�v�@�~�@�~�@�v�@�n�@�E�@��@�@��@�b@���@�o@�@�+@�S�@��@���@��@��T@���@�?}@�Q�@�
=@�=q@�@�?}@�`B@�/@�G�@�j@��@�\)@�;d@�o@�
=@���@��!@���@��T@���@��@��j@��@�l�@�@�J@���@��-@�7L@�9X@��;@�|�@�K�@�
=@��H@���@���@�n�@�=q@�=q@��^@�7L@���@��9@���@�I�@�l�@�@��@��R@�~�@�$�@��h@�G�@��@��u@�A�@�b@��m@��w@�t�@�S�@�K�@�;d@�33@�+@�o@�
=@�
=@�o@��R@�=q@��@���@���@���@��h@�%@���@��9@�9X@��m@��@�C�@�+@�
=@��R@�v�@��@���@��@�hs@�/@���@��@��@�r�@�Q�@�1'@�1'@�b@�  @�1@� �@���@��F@��P@���@���@���@�\)@��@���@�n�@�M�@�M�@�E�@�-@�-@�$�@�@���@�G�@��@��j@�I�@�  @��
@���@�t�@�C�@�
=@�ȴ@��!@���@��\@�~�@�ff@�^5@�V@�V@�M�@�M�@�=q@�5?@��@�x�@�/@��@��@���@�j@�Q�@� �@��;@��@��@�+@�
=@��@��@���@�n�@�-@�J@��#@��@�O�@��@�%@��`@��9@�j@� �@��@���@�K�@�33@�o@���@��H@��+@�ff@�V@�-@���@���@�7L@���@�z�@�I�@��m@��@��P@�t�@�S�@�
=@���@�ff@�J@��-@�`B@���@�z�@��@�@~�R@~��@~v�@~5?@}@}?}@|�j@{�F@z�@z�\@y��@y�@y�^@y�@x��@x��@x �@w��@w|�@w\)@v�R@vV@up�@uV@t��@t�/@tj@sC�@r�\@r-@q7L@p�9@pr�@o�;@o;d@n�@nV@m��@l�@lZ@kS�@j�@j�\@jM�@i�#@i��@ix�@iG�@i�@i%@h��@h�u@hA�@g
=@f��@fff@f{@e/@d�j@d�@d��@d�D@dz�@dz�@cƨ@co@bn�@b=q@a�#@a7L@`��@`r�@`A�@_�@_K�@^ff@^@]��@]��@]��@]�-@]�h@]`B@]/@]�@\�@\��@\��@\Z@\(�@[��@[�
@[�F@[��@[��@[�@[S�@[o@Z�\@Z=q@Y��@Y��@Yx�@YX@YX@YG�@Y&�@Y�@Y%@X��@X�`@X��@X��@X�9@XQ�@X  @W��@W�@W�@W|�@W\)@WK�@V��@V��@VV@U@T�@TZ@T�@S�
@S�@SC�@S"�@R�H@Rn�@RJ@Q��@Q7L@Q%@P��@P��@O��@Ol�@O;d@N��@N��@N5?@M�-@M?}@L��@L�@L�/@L9X@K�m@K�
@K�
@Kƨ@K��@K�@K�@KC�@K33@K"�@K"�@Ko@I�@H�9@H�@HbN@H �@H  @G��@G�P@F�y@F�+@Fv�@FE�@F{@E�-@D�@Dz�@C��@C�F@C�@C@B��@B~�@B^5@BM�@B-@A�@A�7@Ax�@@��@@ �@?+@>��@>�R@>v�@>{@=��@=�h@=�@=�@<��@<��@<Z@;��@:�H@:��@9��@9��@8�`@8�`@8�9@8�u@8r�@81'@81'@81'@8 �@8  @7��@6�R@6�+@6V@6E�@6$�@5�T@5�@5?}@4��@4z�@49X@3��@3�
@3�F@3��@3t�@3S�@3"�@2�H@2��@2�\@2^5@2=q@2�@1��@1�#@1��@1X@1&�@0��@0�u@0�@0A�@0  @/�@.�y@.ȴ@.V@-�-@-`B@,��@,�@,�/@,��@,�D@+��@+�
@+�
@+ƨ@+�F@+��@+C�@*�!@)��@)�7@)x�@)X@)X@)G�@)7L@)%@(�`@(��@(�9@(r�@(A�@(  @'|�@';d@&��@&��@&ff@&V@&5?@&@%�T@%�T@%�T@%�T@%�T@%��@%��@%`B@%/@$��@$Z@$(�@$1@#��@#�m@#ƨ@#��@#��@#�@#S�@#33@#o@#o@#o@#@"�@"�\@"-@!�^@!��@!�7@!hs@!%@ ��@ �9@ r�@ Q�@ b@�@�;@�w@;d@K�@;d@�@�y@ȴ@��@��@��@�+@v�@ff@E�@�@�-@�@O�@�/@�@�D@z�@�@ƨ@S�@�@�H@�!@�\@n�@�@�#@�#@��@��@G�@&�@r�@A�@b@�@��@��@l�@K�@�@�y@ff@�@��@?}@�/@�@��@j@(�@1@ƨ@dZ@33@@�!@~�@n�@^5@�#@x�@G�@%@Ĝ@�9@�u@Q�@�;@�@\)@�@�y@�@�@ȴ@ȴ@ȴ@ȴ@��@�@��@��@�h@�h@?}@�/@��@��@�@j@�@�m@ƨ@�F@��@S�@S�@C�@"�@@
�@
�H@
��@
~�@
n�@
=q@	��@	�@	�#@	�#@	��@	��@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�{A��;AЁA�9XA�$�A�A��mA�Aϟ�AϓuAύPAρA�z�A�x�A�r�A�n�A�n�A�hsA�ffA�dZA�`BAͩ�A�+A�E�A��
A�9XA�E�A�?}A�p�A��Aư!A�~�A�A�{A�A�jA��mA�^5A�A���A�v�A� �A�A�A��A�Q�A�A��A���A�dZA��jA���A�XA���A��DA�dZA�C�A���A��hA��#A��A�ZA��yA�`BA�
=A���A�1'A�dZA��A���A�  A���A��A�1'A��FA��PA��A���A��A�"�A��A�n�A��A��HA�ĜA��^A���A��DA�VA��HA��yA��\A�ZA�hsA��+A�v�A�ZA��-A���A�XA�;dA�oA���A�XA���A�1A� �A��^A��wA��A�  A��A�ĜA���A�&�A}�Ax�At��Aq7LAm33Ak&�Ag/Ac�TA_?}A]t�A\�AZ1'AWAS&�AQp�AQ"�AQ�AQ�AQAP�AK�-AH1AF��AE�FAE\)AD�jAAl�A?XA=
=A:A�A8  A6(�A4�yA3ƨA3oA1�A0A�A//A.{A-?}A,Q�A+C�A)��A)�A(�`A(��A'�A&-A$VA$�A#�mA#��A#`BA"��A"VA!K�A�hA"�A��A\)AG�A�PA�/A��A�+A=qA�mA��A�!A�wA��A�;AJA�A-A�A��A��AXA�A��A
VA�-A
=An�Al�A�AjAVAE�A �A�PAAC�A?}Ap�@�ƨ@�n�@�%@��7@��
@�K�@��H@�ff@��@��@�@�@���@�-@�Q�@�7L@�l�@�-@�7L@�j@�j@�  @���@�v�@�O�@���@ޟ�@���@�%@�=q@��@؃@�1'@�;d@��@��T@��#@���@�ff@��/@�v�@ѡ�@��#@Ѳ-@�G�@��@��@�%@ЋD@��;@���@��#@�G�@̃@��@��@�{@ɑh@�Z@ǝ�@ư!@ļj@�b@���@ÍP@§�@���@���@�bN@���@�ȴ@��+@�E�@���@���@�/@��@��;@��y@���@��^@��/@���@��@�bN@�1@���@��@�K�@�S�@�"�@�
=@��@��\@�~�@�v�@�~�@�~�@�v�@�n�@�E�@��@�@��@�b@���@�o@�@�+@�S�@��@���@��@��T@���@�?}@�Q�@�
=@�=q@�@�?}@�`B@�/@�G�@�j@��@�\)@�;d@�o@�
=@���@��!@���@��T@���@��@��j@��@�l�@�@�J@���@��-@�7L@�9X@��;@�|�@�K�@�
=@��H@���@���@�n�@�=q@�=q@��^@�7L@���@��9@���@�I�@�l�@�@��@��R@�~�@�$�@��h@�G�@��@��u@�A�@�b@��m@��w@�t�@�S�@�K�@�;d@�33@�+@�o@�
=@�
=@�o@��R@�=q@��@���@���@���@��h@�%@���@��9@�9X@��m@��@�C�@�+@�
=@��R@�v�@��@���@��@�hs@�/@���@��@��@�r�@�Q�@�1'@�1'@�b@�  @�1@� �@���@��F@��P@���@���@���@�\)@��@���@�n�@�M�@�M�@�E�@�-@�-@�$�@�@���@�G�@��@��j@�I�@�  @��
@���@�t�@�C�@�
=@�ȴ@��!@���@��\@�~�@�ff@�^5@�V@�V@�M�@�M�@�=q@�5?@��@�x�@�/@��@��@���@�j@�Q�@� �@��;@��@��@�+@�
=@��@��@���@�n�@�-@�J@��#@��@�O�@��@�%@��`@��9@�j@� �@��@���@�K�@�33@�o@���@��H@��+@�ff@�V@�-@���@���@�7L@���@�z�@�I�@��m@��@��P@�t�@�S�@�
=@���@�ff@�J@��-@�`B@���@�z�@��@�@~�R@~��@~v�@~5?@}@}?}@|�j@{�F@z�@z�\@y��@y�@y�^@y�@x��@x��@x �@w��@w|�@w\)@v�R@vV@up�@uV@t��@t�/@tj@sC�@r�\@r-@q7L@p�9@pr�@o�;@o;d@n�@nV@m��@l�@lZ@kS�@j�@j�\@jM�@i�#@i��@ix�@iG�@i�@i%@h��@h�u@hA�@g
=@f��@fff@f{@e/@d�j@d�@d��@d�D@dz�@dz�@cƨ@co@bn�@b=q@a�#@a7L@`��@`r�@`A�@_�@_K�@^ff@^@]��@]��@]��@]�-@]�h@]`B@]/@]�@\�@\��@\��@\Z@\(�@[��@[�
@[�F@[��@[��@[�@[S�@[o@Z�\@Z=q@Y��@Y��@Yx�@YX@YX@YG�@Y&�@Y�@Y%@X��@X�`@X��@X��@X�9@XQ�@X  @W��@W�@W�@W|�@W\)@WK�@V��@V��@VV@U@T�@TZ@T�@S�
@S�@SC�@S"�@R�H@Rn�@RJ@Q��@Q7L@Q%@P��@P��@O��@Ol�@O;d@N��@N��@N5?@M�-@M?}@L��@L�@L�/@L9X@K�m@K�
@K�
@Kƨ@K��@K�@K�@KC�@K33@K"�@K"�@Ko@I�@H�9@H�@HbN@H �@H  @G��@G�P@F�y@F�+@Fv�@FE�@F{@E�-@D�@Dz�@C��@C�F@C�@C@B��@B~�@B^5@BM�@B-@A�@A�7@Ax�@@��@@ �@?+@>��@>�R@>v�@>{@=��@=�h@=�@=�@<��@<��@<Z@;��@:�H@:��@9��@9��@8�`@8�`@8�9@8�u@8r�@81'@81'@81'@8 �@8  @7��@6�R@6�+@6V@6E�@6$�@5�T@5�@5?}@4��@4z�@49X@3��@3�
@3�F@3��@3t�@3S�@3"�@2�H@2��@2�\@2^5@2=q@2�@1��@1�#@1��@1X@1&�@0��@0�u@0�@0A�@0  @/�@.�y@.ȴ@.V@-�-@-`B@,��@,�@,�/@,��@,�D@+��@+�
@+�
@+ƨ@+�F@+��@+C�@*�!@)��@)�7@)x�@)X@)X@)G�@)7L@)%@(�`@(��@(�9@(r�@(A�@(  @'|�@';d@&��@&��@&ff@&V@&5?@&@%�T@%�T@%�T@%�T@%�T@%��@%��@%`B@%/@$��@$Z@$(�@$1@#��@#�m@#ƨ@#��@#��@#�@#S�@#33@#o@#o@#o@#@"�@"�\@"-@!�^@!��@!�7@!hs@!%@ ��@ �9@ r�@ Q�@ b@�@�;@�w@;d@K�@;d@�@�y@ȴ@��@��@��@�+@v�@ff@E�@�@�-@�@O�@�/@�@�D@z�@�@ƨ@S�@�@�H@�!@�\@n�@�@�#@�#@��@��@G�@&�@r�@A�@b@�@��@��@l�@K�@�@�y@ff@�@��@?}@�/@�@��@j@(�@1@ƨ@dZ@33@@�!@~�@n�@^5@�#@x�@G�@%@Ĝ@�9@�u@Q�@�;@�@\)@�@�y@�@�@ȴ@ȴ@ȴ@ȴ@��@�@��@��@�h@�h@?}@�/@��@��@�@j@�@�m@ƨ@�F@��@S�@S�@C�@"�@@
�@
�H@
��@
~�@
n�@
=q@	��@	�@	�#@	�#@	��@	��@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B{BoBuBuBuB{BuBuBoBoBoBoBoBoBoBoBoBuB�BQ�Bt�B��B��B�B��B�
B�`B��B��B��B�B(�B=qBG�BP�BT�BYB]/BgmB_;BO�BO�BdZB{�B��B�BB��B�BB�yB�B��B��B��B��B��B��B�B��BBBB1BPB�B�B�BPBJBhB��B��B��B�jB�BbNBR�BH�BA�B/B�B{BuBhB\B
=B�B�B�jB�9B��B�oB{�BS�B1'B�B�B�BB
�B
�B
��B
�B
�BB
ƨB
�?B
��B
�DB
iyB
S�B
F�B
=qB
/B
bB	��B	�;B	B	�?B	��B	�7B	k�B	_;B	^5B	Q�B	F�B	49B	(�B	%�B	%�B	$�B	#�B	!�B	oB��B��B�B�B�B�mB�#B��B��BÖB�wB�dB�LB�?B�-B�B��B��B��B��B��B��B��B��B��B�oB�bB�PB�JB�JB�DB�=B�7B�1B�1B�B�B�B�B�B}�B|�B{�B{�B{�Bz�By�Bx�Bw�Bv�Bt�Bt�Bp�Bo�Bn�Bn�Bm�Bm�Bl�Bl�Bk�Bo�Bn�Bo�Bs�Bu�Bu�Bu�Bu�Bv�Bz�B�B��B��B�!B�-B�!B�9B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�B�3B�LB�jB�qB�qB�qB�qB�jB�jB�dB�jB��B��BÖBǮBȴBȴBɺB��B��B��B��B��B�B�HB�TB�fB�B��B��B��B��B	  B	B	B	1B	DB	JB	\B	bB	{B	�B	�B	 �B	"�B	$�B	-B	.B	/B	1'B	7LB	9XB	9XB	<jB	?}B	B�B	B�B	C�B	C�B	E�B	G�B	J�B	L�B	R�B	VB	VB	YB	\)B	aHB	ffB	k�B	n�B	q�B	q�B	r�B	r�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	s�B	s�B	t�B	v�B	x�B	z�B	|�B	|�B	{�B	{�B	}�B	�B	�7B	�PB	�bB	�hB	�hB	�oB	�uB	�uB	�oB	�hB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�9B	�9B	�FB	�LB	�XB	�^B	�jB	�wB	�}B	�}B	�}B	��B	��B	��B	B	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�ZB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
VB
VB
\B
bB
hB
hB
uB
{B
{B
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
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
^5B
_;B
_;B
`BB
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
bNB
aHB
aHB
aHB
cTB
dZB
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
e`B
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
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
r�B
r�B
s�B
s�B
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
x�B
x�B
x�B
x�B
y�B
y�B
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
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
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
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
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
�PB
�PB
�PB
�PB
�PB
�VB
�VB
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
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
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
��3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333   B�B�B�B{BoBuBuBuB{BuBuBoBoBoBoBoBoBoBoBoBuB�BQ�Bt�B��B��B�B��B�
B�`B��B��B��B�B(�B=qBG�BP�BT�BYB]/BgmB_;BO�BO�BdZB{�B��B�BB��B�BB�yB�B��B��B��B��B��B��B�B��BBBB1BPB�B�B�BPBJBhB��B��B��B�jB�BbNBR�BH�BA�B/B�B{BuBhB\B
=B�B�B�jB�9B��B�oB{�BS�B1'B�B�B�BB
�B
�B
��B
�B
�BB
ƨB
�?B
��B
�DB
iyB
S�B
F�B
=qB
/B
bB	��B	�;B	B	�?B	��B	�7B	k�B	_;B	^5B	Q�B	F�B	49B	(�B	%�B	%�B	$�B	#�B	!�B	oB��B��B�B�B�B�mB�#B��B��BÖB�wB�dB�LB�?B�-B�B��B��B��B��B��B��B��B��B��B�oB�bB�PB�JB�JB�DB�=B�7B�1B�1B�B�B�B�B�B}�B|�B{�B{�B{�Bz�By�Bx�Bw�Bv�Bt�Bt�Bp�Bo�Bn�Bn�Bm�Bm�Bl�Bl�Bk�Bo�Bn�Bo�Bs�Bu�Bu�Bu�Bu�Bv�Bz�B�B��B��B�!B�-B�!B�9B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�B�3B�LB�jB�qB�qB�qB�qB�jB�jB�dB�jB��B��BÖBǮBȴBȴBɺB��B��B��B��B��B�B�HB�TB�fB�B��B��B��B��B	  B	B	B	1B	DB	JB	\B	bB	{B	�B	�B	 �B	"�B	$�B	-B	.B	/B	1'B	7LB	9XB	9XB	<jB	?}B	B�B	B�B	C�B	C�B	E�B	G�B	J�B	L�B	R�B	VB	VB	YB	\)B	aHB	ffB	k�B	n�B	q�B	q�B	r�B	r�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	s�B	s�B	t�B	v�B	x�B	z�B	|�B	|�B	{�B	{�B	}�B	�B	�7B	�PB	�bB	�hB	�hB	�oB	�uB	�uB	�oB	�hB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�9B	�9B	�FB	�LB	�XB	�^B	�jB	�wB	�}B	�}B	�}B	��B	��B	��B	B	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�ZB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
VB
VB
\B
bB
hB
hB
uB
{B
{B
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
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
^5B
_;B
_;B
`BB
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
bNB
aHB
aHB
aHB
cTB
dZB
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
e`B
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
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
r�B
r�B
s�B
s�B
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
x�B
x�B
x�B
x�B
y�B
y�B
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
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
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
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
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
�PB
�PB
�PB
�PB
�PB
�VB
�VB
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
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
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
��3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230502184215  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230502094230  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230502094231  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230502094234                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230502094234  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230502094234  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230502095901                      G�O�G�O�G�O�                