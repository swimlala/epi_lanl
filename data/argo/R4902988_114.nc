CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-24T00:48:55Z creation;2022-11-24T00:48:56Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221124004855  20221124010835  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @� 8:7_21   @� 8��[�@;d�/���d���F1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A���A�ffB��B  B  B   B(  B0  B8  B@ffBH  BP  BX  B_��Bh  Bp  BxffB�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B���B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C#�fC&  C(  C*  C,  C.�C0  C1�fC4  C6  C7�fC:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C��C��C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��C�  D   D � D  D� D  D� D  D�fDfD�fD  D� D  Dy�D��D� D  D� D	  D	� D
  D
�fD  D� D  D� D��D� DfD� D  D� D��D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D7��D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>�fD?fD?� D@  D@� DA  DA� DBfDB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK�fDL  DL� DMfDM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf�fDg  Dgy�Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D��3D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D���D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�C3D D¼�D���D�<�DÀ D�� D�  D�<�D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ Dɼ�D���D�@ Dʀ D�� D�  D�@ D�|�D˼�D���D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�<�D߀ D�� D�  D�C3D��3D��3D�3D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�<�D�|�D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A�z�A��A��A��AϮA߮A�z�A�zBp�B�
B�
B�
B'�
B/�
B7�
B@=pBG�
BO�
BW�
B_p�Bg�
Bo�
Bx=pB�
B��B��B��B��RB��B��B��RB��RB��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��C]C��C��C��C	��C��C��C��C��C��C��C��C��C]C��C��C!��C#�)C%��C'��C)��C+��C.]C/��C1�)C3��C5��C7�)C9��C;��C=��C?�)CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CX]CZ]C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�)C{��C}��C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C��C��C���C��C��C��C���C���C���C��C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C��C��C���C���C��C��C��C��C���C���C���C��C���C���C���C��C���C���C���C���C���C���C���C���C��C���C���C���C���C��C��C���C��C��C���C��C���C���C���C���C��C��C���C��C��C���C���D }qD �qD}qD�qD}qD�qD��D�D��D�qD}qD�qDwD�D}qD�qD}qD�qD	}qD	�qD
��D
�qD}qD�qD}qD�D}qD�D}qD�qD}qD�D}qD�qD}qD�qDwD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&��D&�qD'}qD'�qD(}qD)�D)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD0�D0}qD0�qD1}qD1�qD2��D2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD7�D7}qD7�D8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�D<}qD<�qD=}qD=�qD>��D?�D?}qD?�qD@}qD@�qDA}qDB�DB��DB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJwDJ�qDK��DK�qDL}qDM�DM��DM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDXwDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\wD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDf�Df��Df�qDgwDg�qDh}qDi�Di}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�A�D�~�D���D���D�;�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�;�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D��D�A�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D��D�>�D�~�D���D���D�;�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�{�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�{�D���D���D�A�D�~�D»�D���D�;�D�~�Dþ�D���D�;�D�{�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɻ�D���D�>�D�~�Dʾ�D���D�>�D�{�D˻�D���D�>�D�~�D̾�D���D�;�D�~�D;�D���D�>�D�~�Dξ�D��D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D܁�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�;�D�~�D߾�D���D�A�D���D���D��D�>�D�{�DễD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D仅D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D���D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�;�D�{�D�D��D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��A���A��2A���A���A���A��A��AA��ZA��|A���A���A��A���A���A���A��A��A�oA�A��A�  A��]A�
�A��A��A�uA��A��A��A�DA�SA�A��A��A���Aȹ$AȰ�AȠ�A�s�A��A�1�A�qA�`A�P�A�}�A�+kA��A�6A��tA��sA��2A�6�A�_A�}"A��%A���A��?A�4A��'A�{�A�A��>A��A�DgA���A��A�z�A��A���A�<A�/�A��+A��A��A��CA�bA�RTA�:*A�ٴA��A��4A�GA�+kA�m)A�.IA�'�A��lA�K^A�ܒA���A�یA�oiA�2�A�YKA�VA���A���A��%A��A��HA��xA�pA�'�A�� A��~A�A�.IA�PHA���A�_�A�� A��xA~��A{��Ax�*Aw=Au��Ar��Ao��AmZ�AkAjT�Ahu�AfuAe�4Ad�ZAb�sAa�!Aa@A`i�A^0UA]�SA]YKA\y>A[��AZMjAX��AWVmAV� AVF�AUC-ASqvAQ��AQGAOZANS&AM�AL֡ALE9AJ��AI�AI��AH�3AG6�AF6�AE)_ACIRAA+�A?��A=$tA:��A8��A8�IA8��A8��A8c A833A7�A7A6:�A4�UA4��A4�\A4u�A3�A2�9A2C-A0�mA.��A,!-A+ �A*�A*�A)�2A)�OA)�A(n/A&��A&�	A%��A%B�A$��A#�ZA"�]A!�:A ��A�A��A��A��AیA%A�A�A��Aj�A�>A�OA�MA�>AW?A�OA=�A�IA�!AcA˒AJ�A��A� AC-A��A	lA`�A��AX�A��A
͟A	��A�4A� A�A��AT�A5?A��A��A�SA�AMjAA�A�SA_pA(�A�A�Al�@�}�@��I@�  @�͟@�x@��d@�/@��!@���@��@��f@��@�l"@�_@�`B@��@�e@�5?@��[@��s@�[W@�s@��@��f@��/@�@��@�0U@��]@�L0@�Mj@���@��@�7@�@�@�O�@Ҋr@�ϫ@�o@У@�;�@��r@�خ@�6z@�V@�*0@���@�Xy@ʺ�@�#:@�c@�A�@��M@���@���@��@�'R@�G�@��_@��{@�@�x@��@� �@���@��H@�m]@�/�@���@��@���@��@��@�xl@��/@���@�?�@���@���@�o @�B�@�@��j@�#:@�a�@���@�n�@�2�@�J@�zx@��@��j@�{@�F@�}V@��K@�?}@��@��@��?@�:*@�T�@�	l@���@�?@���@���@�6z@���@�B[@�$�@��3@��C@��7@�m]@��@�(�@�^�@�e�@�j@��	@��u@�p�@���@�&�@��@�X�@���@�|�@�J#@���@���@�w�@�M@�˒@��4@�B�@�@@���@���@��@�u%@�i�@�@��F@�s@�%F@���@��@�_@�  @���@��
@�خ@���@��S@�|�@�j@�q@��r@�a|@�Z�@�E�@�J@���@��T@��H@���@��@��5@�m�@�\�@�]d@�[�@�B[@� �@��@�{J@��@��@�ی@���@�� @�j@�~@��w@�t�@�H�@��'@�~�@�n�@�M�@�-�@�"h@�M@���@��[@�~�@�B�@��@�>B@�~�@��P@�҉@�˒@��@��o@�j@�Q@�Ft@�8�@��@�_@��@��>@��'@��@��`@�͟@���@��x@�~(@�r�@�ff@�	@�g@S�@�@~��@~-@}x�@}�@|��@|��@|�@|A�@{��@zc @y�D@yw2@yS&@yS&@y8�@x��@x�@x_@xS�@x>B@w��@w)_@v�!@u�)@u��@u�C@uu�@tw�@s�]@s��@s�q@s+@rW�@q��@qhs@p��@p��@p��@p�I@p��@pM@o@nں@n��@n��@np;@n:*@m��@l��@lI�@l�@k�F@ky�@k'�@j�@iԕ@i�@ia�@h�@gƨ@gs@gX�@g=@g@f��@fں@f�6@f��@f^5@f$�@ec�@d��@c�@b��@`��@`1@_�;@_��@_��@_��@_)_@^�y@^i�@^8�@]�=@]J�@]?}@\��@\�j@\�I@\Q�@\Q�@\D�@\(�@\,=@\/�@\%�@\�@[�A@[�
@[��@[ƨ@[�w@[��@[O@[�@Z�b@ZJ�@Yԕ@X�@W�A@W�g@W�a@W�{@W�@V�h@V�1@V��@V��@VkQ@VC�@V@U��@Uq@U@U�@T֡@T��@Ty>@TXy@TD�@T-�@T%�@T~@T�@Tx@S��@S��@S��@S�f@S�F@S��@S�[@S.I@S@R?@Q��@Q�@P��@P%�@O��@O�6@OU�@N�]@N��@N:*@M��@M�7@MT�@M@@L��@L��@L�	@L�)@L�@L�4@Ly>@L1'@L1@Kخ@K��@K��@KU�@K�@J��@Jq�@J;�@J{@I��@I`B@I@I+@I@I�@H��@H��@Hx@G/�@F��@Fi�@F:*@E�@E�H@E?}@D��@D�[@D�[@D֡@D�E@D�E@D~(@CA�@Bh
@B
�@A��@A��@Ae,@@��@@��@@u�@@Xy@@/�@@*�@?�]@?ݘ@?��@?�g@?��@?��@?o�@?9�@>�H@>�6@>u%@>a|@>GE@=�h@<�E@<�z@<Z@<�@;�@;�[@;K�@;=@;o@:�2@:��@:�h@:�h@:�b@:�1@:�+@:kQ@:Q@:B[@:_@97L@8�@8�@8u�@8>B@7�g@7E9@6�@6��@6^5@6#:@5��@5��@5�'@5s�@4�K@4�@4w�@4y>@4q@4V�@4Q�@4'R@3��@3,�@2��@2�6@2��@2}V@2M�@1�H@0�f@0�.@0w�@0Q�@0*�@0@/�@/��@.��@.�b@.��@.~�@.d�@.E�@-��@-+�@,��@,M@,  @+��@+�P@+{J@+F�@*��@*�<@*��@*l�@*6�@)�@)��@)=�@)+@(�	@(��@(�9@(��@(�@(��@(oi@(?�@(<�@(7�@(9X@(4n@(7�@(M@'��@'�*@'��@'x@&��@&Ta@%�@%+�@$��@$�.@$K^@$M@#�W@#ƨ@#�V@#4�@"��@"��@"��@"xl@"ff@"c @"V@"?@!�@!�h@!�@ ��@ �e@ z�@ _@ H@ 4n@ /�@ /�@ /�@ 2�@ 7�@ <�@ (�@��@�@@RT@>�@�@�6@��@s�@h
@a|@Q@J�@0U@�z@��@��@Vm@<6@*0@&�@!�@�@��@�?@��@Q�@4n@7@1@�F@6z@
=@�c@ȴ@�}@�r@Z�@J@e,@;@�u@ �@˒@�V@x@qv@Mj@8@Y@�M@�,@�r@�@�-@p�@�p@z�@Xy@*�@�@�@�@�}@��@�@�@�@u%@0U@	@�.@��@�#@c@hs@`B@<6@q@@@��@�E@Z@4n@��@�a@l�@��@�@��@�!@~�@kQ@V@?@?@=q@?@8�@6�@@�@@ �@�^@�'@J�@*0@�@%@��@��@Ɇ@h�@*�@�q@\)@/�@�@
�@
�@
�L@
�A@
u%@
^5@
&�@	��@	��@	[W@	=�@	4@	�@�@��@ی@�@�@�f@�/@��@�@�0@�P@n/@a@]�@P�@=@.I@+@'�@!-@�@�@�@@��@��@��@��@q�@V@5?@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��A���A��2A���A���A���A��A��AA��ZA��|A���A���A��A���A���A���A��A��A�oA�A��A�  A��]A�
�A��A��A�uA��A��A��A�DA�SA�A��A��A���Aȹ$AȰ�AȠ�A�s�A��A�1�A�qA�`A�P�A�}�A�+kA��A�6A��tA��sA��2A�6�A�_A�}"A��%A���A��?A�4A��'A�{�A�A��>A��A�DgA���A��A�z�A��A���A�<A�/�A��+A��A��A��CA�bA�RTA�:*A�ٴA��A��4A�GA�+kA�m)A�.IA�'�A��lA�K^A�ܒA���A�یA�oiA�2�A�YKA�VA���A���A��%A��A��HA��xA�pA�'�A�� A��~A�A�.IA�PHA���A�_�A�� A��xA~��A{��Ax�*Aw=Au��Ar��Ao��AmZ�AkAjT�Ahu�AfuAe�4Ad�ZAb�sAa�!Aa@A`i�A^0UA]�SA]YKA\y>A[��AZMjAX��AWVmAV� AVF�AUC-ASqvAQ��AQGAOZANS&AM�AL֡ALE9AJ��AI�AI��AH�3AG6�AF6�AE)_ACIRAA+�A?��A=$tA:��A8��A8�IA8��A8��A8c A833A7�A7A6:�A4�UA4��A4�\A4u�A3�A2�9A2C-A0�mA.��A,!-A+ �A*�A*�A)�2A)�OA)�A(n/A&��A&�	A%��A%B�A$��A#�ZA"�]A!�:A ��A�A��A��A��AیA%A�A�A��Aj�A�>A�OA�MA�>AW?A�OA=�A�IA�!AcA˒AJ�A��A� AC-A��A	lA`�A��AX�A��A
͟A	��A�4A� A�A��AT�A5?A��A��A�SA�AMjAA�A�SA_pA(�A�A�Al�@�}�@��I@�  @�͟@�x@��d@�/@��!@���@��@��f@��@�l"@�_@�`B@��@�e@�5?@��[@��s@�[W@�s@��@��f@��/@�@��@�0U@��]@�L0@�Mj@���@��@�7@�@�@�O�@Ҋr@�ϫ@�o@У@�;�@��r@�خ@�6z@�V@�*0@���@�Xy@ʺ�@�#:@�c@�A�@��M@���@���@��@�'R@�G�@��_@��{@�@�x@��@� �@���@��H@�m]@�/�@���@��@���@��@��@�xl@��/@���@�?�@���@���@�o @�B�@�@��j@�#:@�a�@���@�n�@�2�@�J@�zx@��@��j@�{@�F@�}V@��K@�?}@��@��@��?@�:*@�T�@�	l@���@�?@���@���@�6z@���@�B[@�$�@��3@��C@��7@�m]@��@�(�@�^�@�e�@�j@��	@��u@�p�@���@�&�@��@�X�@���@�|�@�J#@���@���@�w�@�M@�˒@��4@�B�@�@@���@���@��@�u%@�i�@�@��F@�s@�%F@���@��@�_@�  @���@��
@�خ@���@��S@�|�@�j@�q@��r@�a|@�Z�@�E�@�J@���@��T@��H@���@��@��5@�m�@�\�@�]d@�[�@�B[@� �@��@�{J@��@��@�ی@���@�� @�j@�~@��w@�t�@�H�@��'@�~�@�n�@�M�@�-�@�"h@�M@���@��[@�~�@�B�@��@�>B@�~�@��P@�҉@�˒@��@��o@�j@�Q@�Ft@�8�@��@�_@��@��>@��'@��@��`@�͟@���@��x@�~(@�r�@�ff@�	@�g@S�@�@~��@~-@}x�@}�@|��@|��@|�@|A�@{��@zc @y�D@yw2@yS&@yS&@y8�@x��@x�@x_@xS�@x>B@w��@w)_@v�!@u�)@u��@u�C@uu�@tw�@s�]@s��@s�q@s+@rW�@q��@qhs@p��@p��@p��@p�I@p��@pM@o@nں@n��@n��@np;@n:*@m��@l��@lI�@l�@k�F@ky�@k'�@j�@iԕ@i�@ia�@h�@gƨ@gs@gX�@g=@g@f��@fں@f�6@f��@f^5@f$�@ec�@d��@c�@b��@`��@`1@_�;@_��@_��@_��@_)_@^�y@^i�@^8�@]�=@]J�@]?}@\��@\�j@\�I@\Q�@\Q�@\D�@\(�@\,=@\/�@\%�@\�@[�A@[�
@[��@[ƨ@[�w@[��@[O@[�@Z�b@ZJ�@Yԕ@X�@W�A@W�g@W�a@W�{@W�@V�h@V�1@V��@V��@VkQ@VC�@V@U��@Uq@U@U�@T֡@T��@Ty>@TXy@TD�@T-�@T%�@T~@T�@Tx@S��@S��@S��@S�f@S�F@S��@S�[@S.I@S@R?@Q��@Q�@P��@P%�@O��@O�6@OU�@N�]@N��@N:*@M��@M�7@MT�@M@@L��@L��@L�	@L�)@L�@L�4@Ly>@L1'@L1@Kخ@K��@K��@KU�@K�@J��@Jq�@J;�@J{@I��@I`B@I@I+@I@I�@H��@H��@Hx@G/�@F��@Fi�@F:*@E�@E�H@E?}@D��@D�[@D�[@D֡@D�E@D�E@D~(@CA�@Bh
@B
�@A��@A��@Ae,@@��@@��@@u�@@Xy@@/�@@*�@?�]@?ݘ@?��@?�g@?��@?��@?o�@?9�@>�H@>�6@>u%@>a|@>GE@=�h@<�E@<�z@<Z@<�@;�@;�[@;K�@;=@;o@:�2@:��@:�h@:�h@:�b@:�1@:�+@:kQ@:Q@:B[@:_@97L@8�@8�@8u�@8>B@7�g@7E9@6�@6��@6^5@6#:@5��@5��@5�'@5s�@4�K@4�@4w�@4y>@4q@4V�@4Q�@4'R@3��@3,�@2��@2�6@2��@2}V@2M�@1�H@0�f@0�.@0w�@0Q�@0*�@0@/�@/��@.��@.�b@.��@.~�@.d�@.E�@-��@-+�@,��@,M@,  @+��@+�P@+{J@+F�@*��@*�<@*��@*l�@*6�@)�@)��@)=�@)+@(�	@(��@(�9@(��@(�@(��@(oi@(?�@(<�@(7�@(9X@(4n@(7�@(M@'��@'�*@'��@'x@&��@&Ta@%�@%+�@$��@$�.@$K^@$M@#�W@#ƨ@#�V@#4�@"��@"��@"��@"xl@"ff@"c @"V@"?@!�@!�h@!�@ ��@ �e@ z�@ _@ H@ 4n@ /�@ /�@ /�@ 2�@ 7�@ <�@ (�@��@�@@RT@>�@�@�6@��@s�@h
@a|@Q@J�@0U@�z@��@��@Vm@<6@*0@&�@!�@�@��@�?@��@Q�@4n@7@1@�F@6z@
=@�c@ȴ@�}@�r@Z�@J@e,@;@�u@ �@˒@�V@x@qv@Mj@8@Y@�M@�,@�r@�@�-@p�@�p@z�@Xy@*�@�@�@�@�}@��@�@�@�@u%@0U@	@�.@��@�#@c@hs@`B@<6@q@@@��@�E@Z@4n@��@�a@l�@��@�@��@�!@~�@kQ@V@?@?@=q@?@8�@6�@@�@@ �@�^@�'@J�@*0@�@%@��@��@Ɇ@h�@*�@�q@\)@/�@�@
�@
�@
�L@
�A@
u%@
^5@
&�@	��@	��@	[W@	=�@	4@	�@�@��@ی@�@�@�f@�/@��@�@�0@�P@n/@a@]�@P�@=@.I@+@'�@!-@�@�@�@@��@��@��@��@q�@V@5?@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�sB�XB�>B��B��B��B��B�B��B��B�B��B��B��B��B��B��B�XB�$B��B�_B�yB�yB�yB��B�_B�_B��B��B��B�XB��B�HB��B�/B��B�"B}�B6�BB�B��B�pB��B�<BǔB�yB��B��B��B�~B� BzDBp�Bk�BiyBc:B^�B]/BM6B7fB6�B=�BF�BA B9�B0�B(�B#�B�BWBDB�qB��B�HB׍B�<B��B��B�_By�BZB&2B�B[B B�;B�B��B�nB�fB�vB�VB�dB�OB��B�KB��B�gB� B��B�#B.Br�B^�BQ�BC�B'�B$ZB�B�B
�OB
�RB
��B
ˬB
�B
��B
��B
�#B
�B
�B
z*B
x�B
nB
e�B
bhB
`�B
UgB
OBB
LdB
I�B
C�B
>�B
7�B
0!B
*�B
(sB
$�B
QB
 B
�B
�B	��B	�B	�>B	�?B	��B	�=B	��B	��B	޸B	��B	��B	��B	�GB	��B	�hB	��B	��B	�IB	�dB	��B	�qB	�7B	�+B	��B	��B	��B	�#B	��B	�fB	��B	� B	~(B	yrB	s�B	h�B	d�B	a�B	bhB	`�B	_�B	\�B	\�B	W?B	U�B	UB	TaB	Q�B	N�B	IlB	E9B	@�B	=<B	:DB	5�B	3MB	0�B	-�B	+�B	+QB	*KB	(
B	"�B	 \B	!B	B	QB	�B	�B	�B	,B	�B	TB	.B	B	�B	~B	�B	
XB		B	�B	�B	B	�B	uB	�B	 �B�}B�wB�]B�(B��B��B�"B��B��B�PB�B�dB�0B��B��B�$B��B�B��B�TB�B�B�MB�B�|B��B�B��B�B� B�GB��B��B�B�iB�B��B��B�B�B�IB��B��B��B�B� B��B�B�wB�OB�B�B��B�+B�zB��B�B�RB��B�lB�XB�*B�jB�B�6B�HB��B��B	 �B	uB	�B	PB	HB	�B	4B	�B	MB	�B	�B	�B	�B	�B	5B	�B	VB	 �B	"�B	# B	%B	(
B	,"B	2�B	9rB	:DB	;0B	<jB	<�B	=qB	=�B	>(B	?}B	I�B	KxB	K�B	MPB	M�B	PbB	QhB	R�B	U�B	V�B	X�B	^5B	`'B	`�B	`BB	`�B	bhB	d�B	e�B	g�B	h�B	i*B	j0B	k�B	m�B	n/B	ncB	oiB	oOB	o�B	o�B	p�B	v+B	z�B	.B	�[B	��B	��B	��B	�B	��B	�B	�B	�B	�&B	�uB	��B	�,B	�B	��B	�B	�B	��B	�4B	��B	�
B	�B	��B	�UB	��B	�hB	�9B	�B	��B	�*B	��B	��B	��B	��B	��B	�HB	�4B	�iB	��B	�'B	żB	��B	��B	�B	�B	��B	�JB	͟B	�VB	ϫB	՛B	�kB	ڠB	ںB	چB	�#B	��B	��B	߾B	�HB	�B	�B	�B	�B	�:B	�B	�2B	�B	�B	�$B	�
B	�
B	��B	�B	�B	��B	��B	�kB	�=B	��B	�}B	�ZB	��B
mB
	7B
�B
�B
SB
�B
�B
$B
YB
�B
+B
B
+B
�B
	B
�B
�B
]B
B
�B
�B
#TB
(�B
(�B
(�B
(�B
+�B
-�B
/�B
1B
1vB
1�B
2-B
2�B
4�B
7�B
8�B
:�B
;�B
;�B
<6B
>]B
@�B
A B
A;B
A;B
B�B
DMB
E�B
H�B
JXB
JrB
K�B
P�B
RoB
R�B
S&B
U2B
Y1B
ZQB
\�B
^jB
_;B
_VB
_�B
_�B
aB
c�B
d�B
d�B
e,B
e�B
e�B
h$B
l�B
m�B
n�B
o�B
o�B
p�B
u�B
w�B
xB
z�B
~wB
�UB
��B
��B
�B
��B
��B
�3B
��B
��B
�B
�9B
�B
�B
��B
��B
�HB
�TB
��B
��B
��B
�B
��B
��B
�B
��B
��B
��B
�/B
�pB
��B
��B
�4B
�NB
��B
�:B
�:B
�:B
�nB
��B
�@B
��B
��B
��B
��B
�B
�B
��B
��B
�XB
��B
��B
��B
�5B
�iB
�!B
��B
��B
��B
�3B
�3B
��B
��B
�B
�tB
�zB
��B
��B
��B
��B
��B
�B
�8B
��B
��B
��B
��B
��B
�	B
�rB
��B
��B
��B
�>B
�XB
�B
�B
��B
�]B
�}B
��B
�aB
ðB
�B
�9B
�YB
��B
ǔB
ȚB
��B
�lB
ɺB
��B
��B
�	B
�rB
ʌB
ʦB
��B
ˬB
��B
�JB
̘B
�6B
�jB
�VB
�(B
ϑB
��B
�B
�4B
��B
� B
�:B
�TB
�TB
ңB
��B
�,B
�B
�$B
��B
��B
��B
��B
�7B
�	B
�	B
�	B
��B
��B
ںB
�=B
�jB
��B
�BB
�vB
��B
�bB
�B
�B
� B
�nB
�B
�B
�&B
�ZB
�ZB
�tB
��B
�,B
�FB
�B
�fB
��B
�RB
�mB
�RB
�B
�0B
�eB
�B
�kB
�B
�B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�B
�IB
�}B
�IB
��B
�!B
��B
�B
��B
�'B
�B
�B
��B
�B
��B
�TB
��B
��B
��B
�%B
�+B
��B
��B
��B
��B
�B
��B
�2B
�lB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�<B
�VB
��B
�B
�wB
��B  B  B B 4B B�B�B'B�B�BGB{B{B�B�BgBMBB�B�B%BtB�B�B+BzBEBzB�B�BB1B1B1BKB1B�B�B	B�B	B
=B
=BBJB�B6B�B�B<BVBpBB�B�B�BB.B.B.B.B�B4BBoB�B�B�B�BB&B&B&B&B&B&B&B�B�B{BFB�BMBgB�B�B�B�B�B�BmBmB�B�B$B?B$B?BYBsB�BEByB�B�B�BBBQB�B�B�B	B#B�B�B�B�B5B�B�B!B!BVBpB�B�B�B BB �B!-B!�B"�B#B#B#nB#�B#�B#�B#�B#�B$�B$�B%,B%�B%�B&LB&2B&2B&�B'B'B'B'8B'mB'�B'�B'�B(�B(�B(�B)B)�B*eB*eB*�B*�B+B+B+6B+kB+QB+kB+QB+QB+kB+�B+�B+�B+�B,"B,"B,�B,�B-B-)B-)B-B-wB-�B./B.�B/OB/iB/�B/�B/�B0!B0oB0�B0�B0�B0�B1�B1�B2B1�B2B2|B2�B2�B2|B2aB2GB2|B3B3�B4nB4�B4�B4�B4�B4�B5%B5%B5?B5?B5ZB5ZB5tB5tB5tB5�B5�B5�B6B6FB6`B6zB6�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B��B��B��B��B��B��B��B�sB�XB�>B��B��B��B��B�B��B��B�B��B��B��B��B��B��B�XB�$B��B�_B�yB�yB�yB��B�_B�_B��B��B��B�XB��B�HB��B�/B��B�"B}�B6�BB�B��B�pB��B�<BǔB�yB��B��B��B�~B� BzDBp�Bk�BiyBc:B^�B]/BM6B7fB6�B=�BF�BA B9�B0�B(�B#�B�BWBDB�qB��B�HB׍B�<B��B��B�_By�BZB&2B�B[B B�;B�B��B�nB�fB�vB�VB�dB�OB��B�KB��B�gB� B��B�#B.Br�B^�BQ�BC�B'�B$ZB�B�B
�OB
�RB
��B
ˬB
�B
��B
��B
�#B
�B
�B
z*B
x�B
nB
e�B
bhB
`�B
UgB
OBB
LdB
I�B
C�B
>�B
7�B
0!B
*�B
(sB
$�B
QB
 B
�B
�B	��B	�B	�>B	�?B	��B	�=B	��B	��B	޸B	��B	��B	��B	�GB	��B	�hB	��B	��B	�IB	�dB	��B	�qB	�7B	�+B	��B	��B	��B	�#B	��B	�fB	��B	� B	~(B	yrB	s�B	h�B	d�B	a�B	bhB	`�B	_�B	\�B	\�B	W?B	U�B	UB	TaB	Q�B	N�B	IlB	E9B	@�B	=<B	:DB	5�B	3MB	0�B	-�B	+�B	+QB	*KB	(
B	"�B	 \B	!B	B	QB	�B	�B	�B	,B	�B	TB	.B	B	�B	~B	�B	
XB		B	�B	�B	B	�B	uB	�B	 �B�}B�wB�]B�(B��B��B�"B��B��B�PB�B�dB�0B��B��B�$B��B�B��B�TB�B�B�MB�B�|B��B�B��B�B� B�GB��B��B�B�iB�B��B��B�B�B�IB��B��B��B�B� B��B�B�wB�OB�B�B��B�+B�zB��B�B�RB��B�lB�XB�*B�jB�B�6B�HB��B��B	 �B	uB	�B	PB	HB	�B	4B	�B	MB	�B	�B	�B	�B	�B	5B	�B	VB	 �B	"�B	# B	%B	(
B	,"B	2�B	9rB	:DB	;0B	<jB	<�B	=qB	=�B	>(B	?}B	I�B	KxB	K�B	MPB	M�B	PbB	QhB	R�B	U�B	V�B	X�B	^5B	`'B	`�B	`BB	`�B	bhB	d�B	e�B	g�B	h�B	i*B	j0B	k�B	m�B	n/B	ncB	oiB	oOB	o�B	o�B	p�B	v+B	z�B	.B	�[B	��B	��B	��B	�B	��B	�B	�B	�B	�&B	�uB	��B	�,B	�B	��B	�B	�B	��B	�4B	��B	�
B	�B	��B	�UB	��B	�hB	�9B	�B	��B	�*B	��B	��B	��B	��B	��B	�HB	�4B	�iB	��B	�'B	żB	��B	��B	�B	�B	��B	�JB	͟B	�VB	ϫB	՛B	�kB	ڠB	ںB	چB	�#B	��B	��B	߾B	�HB	�B	�B	�B	�B	�:B	�B	�2B	�B	�B	�$B	�
B	�
B	��B	�B	�B	��B	��B	�kB	�=B	��B	�}B	�ZB	��B
mB
	7B
�B
�B
SB
�B
�B
$B
YB
�B
+B
B
+B
�B
	B
�B
�B
]B
B
�B
�B
#TB
(�B
(�B
(�B
(�B
+�B
-�B
/�B
1B
1vB
1�B
2-B
2�B
4�B
7�B
8�B
:�B
;�B
;�B
<6B
>]B
@�B
A B
A;B
A;B
B�B
DMB
E�B
H�B
JXB
JrB
K�B
P�B
RoB
R�B
S&B
U2B
Y1B
ZQB
\�B
^jB
_;B
_VB
_�B
_�B
aB
c�B
d�B
d�B
e,B
e�B
e�B
h$B
l�B
m�B
n�B
o�B
o�B
p�B
u�B
w�B
xB
z�B
~wB
�UB
��B
��B
�B
��B
��B
�3B
��B
��B
�B
�9B
�B
�B
��B
��B
�HB
�TB
��B
��B
��B
�B
��B
��B
�B
��B
��B
��B
�/B
�pB
��B
��B
�4B
�NB
��B
�:B
�:B
�:B
�nB
��B
�@B
��B
��B
��B
��B
�B
�B
��B
��B
�XB
��B
��B
��B
�5B
�iB
�!B
��B
��B
��B
�3B
�3B
��B
��B
�B
�tB
�zB
��B
��B
��B
��B
��B
�B
�8B
��B
��B
��B
��B
��B
�	B
�rB
��B
��B
��B
�>B
�XB
�B
�B
��B
�]B
�}B
��B
�aB
ðB
�B
�9B
�YB
��B
ǔB
ȚB
��B
�lB
ɺB
��B
��B
�	B
�rB
ʌB
ʦB
��B
ˬB
��B
�JB
̘B
�6B
�jB
�VB
�(B
ϑB
��B
�B
�4B
��B
� B
�:B
�TB
�TB
ңB
��B
�,B
�B
�$B
��B
��B
��B
��B
�7B
�	B
�	B
�	B
��B
��B
ںB
�=B
�jB
��B
�BB
�vB
��B
�bB
�B
�B
� B
�nB
�B
�B
�&B
�ZB
�ZB
�tB
��B
�,B
�FB
�B
�fB
��B
�RB
�mB
�RB
�B
�0B
�eB
�B
�kB
�B
�B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�B
�IB
�}B
�IB
��B
�!B
��B
�B
��B
�'B
�B
�B
��B
�B
��B
�TB
��B
��B
��B
�%B
�+B
��B
��B
��B
��B
�B
��B
�2B
�lB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�<B
�VB
��B
�B
�wB
��B  B  B B 4B B�B�B'B�B�BGB{B{B�B�BgBMBB�B�B%BtB�B�B+BzBEBzB�B�BB1B1B1BKB1B�B�B	B�B	B
=B
=BBJB�B6B�B�B<BVBpBB�B�B�BB.B.B.B.B�B4BBoB�B�B�B�BB&B&B&B&B&B&B&B�B�B{BFB�BMBgB�B�B�B�B�B�BmBmB�B�B$B?B$B?BYBsB�BEByB�B�B�BBBQB�B�B�B	B#B�B�B�B�B5B�B�B!B!BVBpB�B�B�B BB �B!-B!�B"�B#B#B#nB#�B#�B#�B#�B#�B$�B$�B%,B%�B%�B&LB&2B&2B&�B'B'B'B'8B'mB'�B'�B'�B(�B(�B(�B)B)�B*eB*eB*�B*�B+B+B+6B+kB+QB+kB+QB+QB+kB+�B+�B+�B+�B,"B,"B,�B,�B-B-)B-)B-B-wB-�B./B.�B/OB/iB/�B/�B/�B0!B0oB0�B0�B0�B0�B1�B1�B2B1�B2B2|B2�B2�B2|B2aB2GB2|B3B3�B4nB4�B4�B4�B4�B4�B5%B5%B5?B5?B5ZB5ZB5tB5tB5tB5�B5�B5�B6B6FB6`B6zB6�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221124004854  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221124004855  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221124004856  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221124004856                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221124004856  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221124004856  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221124010835                      G�O�G�O�G�O�                