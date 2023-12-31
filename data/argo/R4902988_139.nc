CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-31T00:57:54Z creation;2023-07-31T00:57:55Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230731005754  20230731010937  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�>y.Es1   @�>yq��@<V�+J�c�1&�x�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A���A�  A�  A���A�  A���A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��C��C�  C�  C��C��C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  D   D � D  D�fD  D� D  D� DfD� D��Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DKfDK�fDL  DL� DM  DM� DM��DNy�DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�DZ��D[� D\  D\� D]fD]� D^fD^�fD_  D_� D`  D`y�Da  Da� Da��Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Df��Dg� Dh  Dhy�Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt�fDu  Du� Dv  Dv�fDw  Dw� Dx  Dx� Dy  Dy� Dy��Dz� D{  D{�fD|fD|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D���D���D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D���D�<�D�|�D�� D�  D�C3Dƀ DƼ�D���D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�<�D�|�D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�<�D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D��3D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A�z�A��A��A�z�AϮA�z�A�B =pB=pB�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��RB��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�)C��C��C��C	��C��C��C��C]C��C��C��C��C��C��C��C!��C#��C&]C'��C)��C+��C-��C/��C1��C3��C5��C8]C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Cj]Cl]Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��C���C���C��C���C���C���C���C���C���C��C���C��C���C���C��C��C��C���C���C��C��C���C���C���C��C��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C��C���C���C��C���C���C��C���C���C���C��C���C��C���C��C���C���C���C��C��C���C���C��C���C��C���C���C���C��C���C��C���C��C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C��C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���C��C��C���C���C���C���C��C���C���C���C��C���C���C���D }qD �qD��D�qD}qD�qD}qD�D}qD�DwD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-��D.�D.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�D=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC��DC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI��DI�qDJ}qDK�DK��DK�qDL}qDL�qDM}qDM�DNwDN�qDO}qDO�qDP��DP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZwDZ�D[}qD[�qD\}qD]�D]}qD^�D^��D^�qD_}qD_�qD`wD`�qDa}qDa�Db}qDb�qDc}qDc�qDdwDd�qDe}qDe�qDf}qDf�Dg}qDg�qDhwDh�qDiwDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDt�Dt��Dt�qDu}qDu�qDv��Dv�qDw}qDw�qDx}qDx�qDy}qDy�Dz}qDz�qD{��D|�D|��D|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D���D���D���D�A�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�{�D¾�D���D�>�D�~�Dþ�D���D�A�D�~�Dľ�D���D�;�D�{�Dž�D���D�A�D�~�Dƻ�D���D�>�Dǁ�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�Dͻ�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�{�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�;�D�{�D߻�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⻅D���D�;�D�{�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D���D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�;�D�~�D���D��D�>�D�~�DD���D�>�D�~�DﾸD���D�A�D�~�D�D���D�>�D�~�D�D���D�>�D��D�D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AΖ�AΟ�A�e�A�[�A�X�A�U�A�L0A�GzA�E9A�;0A�7�A�-�A�&LA��QA��[A���A���A��aA�|PA�YA��yA���A�SA���A�*0A���A�g8A�3�A�7�A�˒A�cA�,=A��:A���A���A�6FA�A�<�A��kA�]dA��A��OA���A�MA�cA��3A�N<A�A�OvA��A��;A�?�A�ŢA��DA�r�A��EA���A�VA�4A��VA��tA��3A�;�A���A�=�A�C�A���A�.A��aA�@�A��A�ޞA���A�%A�oA~�A}ԕA}�A{�Az��Ay>BAw�zAuAs��Asp;Ar��Ar4nAq�[AqF�Ap�Ap��ApN�ApSAoMAm�Ak�SAjH�Ah�Ah33Ag�|AfZ�Ad=�Ab��Ab"hAa1A`^5A` �A_�A_�zA_��A_�A_4A^F�A]X�A\�?A\3�AZ�$AZ�AY�AW^�AUATU2ATAS.IAR��AQc�APJAO�ANjAM�AAL�AKL0AI��AItTAH�$AGԕAG\�AFiDAE�6AD�AD��AD�PADffAC��AC�AAB<�A?خA>ںA>t�A=�NA=�1A=zA=-�A<��A<�A;H�A9u�A8($A6�`A5��A5`�A54A4��A4
�A3�=A3��A3\�A2��A21�A1�/A1v`A0^�A/�7A.�nA.xA..�A-�]A-m�A,D�A*�A)��A)9�A(�^A({A'l�A%m�A#r�A#	A":�A ԕA��A��A,=A�|A|�AC�A�A�A(�A��A%A�IAS&A��A�'A��AuA��A��A?�Ah
A��A>BA�<A��A}�AیA��A
�4A	�vA	�!A	bA�A:*Ap;A��A�At�A1A4A��AYKAA��AK^A4A$�@�zx@��M@���@�~@�h
@��@��@�@�5�@�!�@���@��@��@��@��@���@�@�D�@��@�IR@���@Ⴊ@�,=@�F�@��`@އ+@�\�@�f�@�S�@�<6@�:�@��;@� �@թ*@�(�@ԕ@��@Ҳ�@�;d@�`B@���@ͤ@@ˣn@��@�U2@�!�@��@��W@��Q@Ɂ�@�!�@���@�Mj@�=q@Ï�@�`B@�F�@u@���@�=q@�dZ@��2@��@�G@�4@�	@�!�@�6�@�b�@��2@�Y�@���@��@��<@��}@��@�=�@���@�.I@�l�@�W?@���@���@�s�@��@���@�C@��@�2�@�>�@�� @�@���@�@@��B@���@��.@��7@�:�@��@��@��@�A�@�X�@�Y@�~@��`@�t�@��@��@��@�W�@���@��@��M@�j@���@��@��@��@��\@�l�@�{@��o@���@�6z@���@��!@��9@���@��}@���@���@���@���@��@�J@���@���@�s@�.I@��@� �@���@�X@�0�@�;@�҉@��@�z�@�i�@�C-@��@���@��@���@�f�@�U�@��@��h@�*�@��$@�!�@��@�Ɇ@��L@���@�-�@��3@�X@�"�@��@��v@���@��I@�c @��@�x@�J�@�/�@�/@�&@�+@��@��@��h@���@��+@�r�@�W�@�GE@�ݘ@��-@���@���@�s�@�c�@�?}@�=@��@�҉@��9@��F@�R�@��@�=@�ff@�=q@��@��@�@E9@~��@~_�@~�@}�@}Vm@}#�@|y>@{�
@{��@{j�@{H�@{4�@z��@zc @yzx@y�@x�	@x֡@x~(@xA�@x�@w�;@w��@w�@w4�@v�@v�@u��@u�=@u|@uc�@u?}@u0�@u�@t�@t�U@t�@t(�@sRT@r�h@rkQ@r6�@q��@q��@q��@q�@qu�@q=�@p�@p�@p7@o�4@o(@n��@n^5@m��@m�j@m��@m@@lH@k��@k�6@k_p@j��@jd�@h��@h>B@gA�@f&�@e�C@e�"@eu�@eT�@eIR@e%F@d�_@c��@cC�@b��@b�@b�A@bL0@b1�@b{@a�>@aw2@a7L@`��@`��@`��@`g8@`-�@_��@^��@^{�@]��@]��@]��@\��@[� @[��@[�f@[X�@[J#@[A�@[.I@Z��@Z�,@Z��@Zi�@Z&�@Y�3@Y\�@X�K@X�@X�I@X�I@X2�@W��@W��@W�w@W��@W�0@W��@W��@W��@W�@V҉@V�h@V��@V� @Vq�@VYK@VOv@V:*@U��@U�^@U��@U��@Uu�@U<6@T��@T��@T��@Tl"@S�]@Sݘ@S�K@S��@SZ�@S�@R��@R~�@RJ�@Q�D@Q�)@Q��@QrG@P��@P��@P�@P~@O�@OS�@O33@O�@N�@N��@N@M�^@M�-@M�-@M�S@MIR@L��@L�@L|�@L1'@K��@K�6@K��@Kt�@KF�@K@O@K,�@K&@K�@J�1@JOv@I�9@I��@I#�@H�f@H�K@H�@H֡@H��@G��@G&@F}V@E�o@ErG@E=�@E2a@E-w@E*0@E&�@E�@D��@D�.@D,=@C�;@C��@B�'@B?@A�z@A��@Aw2@@ѷ@@U2@@"h@@�@?�@?iD@?@O@?�@>�@>�@=�-@=j@=%F@<��@<g8@<�@;��@;��@;O@;Y@:��@:�]@:�s@:ȴ@:��@:�b@:�F@:��@:xl@:Ov@:)�@9�T@9�h@9e,@9;@8�Y@7��@7��@7��@7P�@76z@7@6�@6ȴ@6�m@6��@6xl@6s�@6c @6M�@68�@5��@5�@4��@4�@4�@4�Y@3��@3"�@2�A@2?@1�@1f�@0�5@0�@0x@0�@0�@/�Q@/t�@.��@.�x@.5?@.+k@.O@-�@-�X@-��@-a�@-F@-0�@-+@-V@-@-%@,�`@,֡@,��@,PH@+��@+��@+E9@+,�@+"�@*�y@*��@*{�@*@�@*J@)�@)�H@)�=@)��@)}�@)j@)2a@(�?@(��@(�9@(�$@(��@(��@(tT@(A�@(�@'� @'��@'J#@'C@'�@&��@&͟@&�!@&v�@&ff@&)�@%�@%�^@%k�@%!�@%;@$�@$�[@$Ĝ@$�j@$��@$Q�@$:�@$b@#�V@#@O@#'�@#C@#o@#
=@"�@"�@"�2@"��@"Q@"#:@!�@!�@!�X@!��@!�7@!c�@!Dg@ �P@ �u@ _@ I�@ ,=@ b@ �@�r@��@�K@X�@1�@C@�@҉@�@YK@8�@.�@
�@��@��@B�@��@��@`�@%�@�W@{J@E9@"�@o@�"@�h@�@��@ϫ@��@4@�	@�@��@�j@�@z�@j@PH@@�r@�m@�q@4�@�@��@�@��@n�@�@_@��@�@�-@B�@�@	l@;@�U@�@Z@�0@��@J#@�@��@��@��@n�@J�@�.@�9@rG@�@��@�_@bN@:�@خ@e�@U�@�@ߤ@�@5?@e@�@��@�@�@T�@B�@ \@	l@�@�O@�_@��@��@|�@g8@V�@M@N�@�@�Q@��@�{@a@O@/�@�@
�@
��@
��@
�\@
ff@
($@
 �@	ԕ@	@	��@	��@	|@	G�@	Dg@	7L@	(�@	%F@	 \@	�@�@�9@~(@Z@6@@�A@�w@�4@iD@C�@!-@�@�]@�R@��@� @8�@ �@��@��@��@�h@}�@Q�@+@�	@Ĝ@��@M@-�@-�@�@�@�4@~�@n/@RT@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AΖ�AΟ�A�e�A�[�A�X�A�U�A�L0A�GzA�E9A�;0A�7�A�-�A�&LA��QA��[A���A���A��aA�|PA�YA��yA���A�SA���A�*0A���A�g8A�3�A�7�A�˒A�cA�,=A��:A���A���A�6FA�A�<�A��kA�]dA��A��OA���A�MA�cA��3A�N<A�A�OvA��A��;A�?�A�ŢA��DA�r�A��EA���A�VA�4A��VA��tA��3A�;�A���A�=�A�C�A���A�.A��aA�@�A��A�ޞA���A�%A�oA~�A}ԕA}�A{�Az��Ay>BAw�zAuAs��Asp;Ar��Ar4nAq�[AqF�Ap�Ap��ApN�ApSAoMAm�Ak�SAjH�Ah�Ah33Ag�|AfZ�Ad=�Ab��Ab"hAa1A`^5A` �A_�A_�zA_��A_�A_4A^F�A]X�A\�?A\3�AZ�$AZ�AY�AW^�AUATU2ATAS.IAR��AQc�APJAO�ANjAM�AAL�AKL0AI��AItTAH�$AGԕAG\�AFiDAE�6AD�AD��AD�PADffAC��AC�AAB<�A?خA>ںA>t�A=�NA=�1A=zA=-�A<��A<�A;H�A9u�A8($A6�`A5��A5`�A54A4��A4
�A3�=A3��A3\�A2��A21�A1�/A1v`A0^�A/�7A.�nA.xA..�A-�]A-m�A,D�A*�A)��A)9�A(�^A({A'l�A%m�A#r�A#	A":�A ԕA��A��A,=A�|A|�AC�A�A�A(�A��A%A�IAS&A��A�'A��AuA��A��A?�Ah
A��A>BA�<A��A}�AیA��A
�4A	�vA	�!A	bA�A:*Ap;A��A�At�A1A4A��AYKAA��AK^A4A$�@�zx@��M@���@�~@�h
@��@��@�@�5�@�!�@���@��@��@��@��@���@�@�D�@��@�IR@���@Ⴊ@�,=@�F�@��`@އ+@�\�@�f�@�S�@�<6@�:�@��;@� �@թ*@�(�@ԕ@��@Ҳ�@�;d@�`B@���@ͤ@@ˣn@��@�U2@�!�@��@��W@��Q@Ɂ�@�!�@���@�Mj@�=q@Ï�@�`B@�F�@u@���@�=q@�dZ@��2@��@�G@�4@�	@�!�@�6�@�b�@��2@�Y�@���@��@��<@��}@��@�=�@���@�.I@�l�@�W?@���@���@�s�@��@���@�C@��@�2�@�>�@�� @�@���@�@@��B@���@��.@��7@�:�@��@��@��@�A�@�X�@�Y@�~@��`@�t�@��@��@��@�W�@���@��@��M@�j@���@��@��@��@��\@�l�@�{@��o@���@�6z@���@��!@��9@���@��}@���@���@���@���@��@�J@���@���@�s@�.I@��@� �@���@�X@�0�@�;@�҉@��@�z�@�i�@�C-@��@���@��@���@�f�@�U�@��@��h@�*�@��$@�!�@��@�Ɇ@��L@���@�-�@��3@�X@�"�@��@��v@���@��I@�c @��@�x@�J�@�/�@�/@�&@�+@��@��@��h@���@��+@�r�@�W�@�GE@�ݘ@��-@���@���@�s�@�c�@�?}@�=@��@�҉@��9@��F@�R�@��@�=@�ff@�=q@��@��@�@E9@~��@~_�@~�@}�@}Vm@}#�@|y>@{�
@{��@{j�@{H�@{4�@z��@zc @yzx@y�@x�	@x֡@x~(@xA�@x�@w�;@w��@w�@w4�@v�@v�@u��@u�=@u|@uc�@u?}@u0�@u�@t�@t�U@t�@t(�@sRT@r�h@rkQ@r6�@q��@q��@q��@q�@qu�@q=�@p�@p�@p7@o�4@o(@n��@n^5@m��@m�j@m��@m@@lH@k��@k�6@k_p@j��@jd�@h��@h>B@gA�@f&�@e�C@e�"@eu�@eT�@eIR@e%F@d�_@c��@cC�@b��@b�@b�A@bL0@b1�@b{@a�>@aw2@a7L@`��@`��@`��@`g8@`-�@_��@^��@^{�@]��@]��@]��@\��@[� @[��@[�f@[X�@[J#@[A�@[.I@Z��@Z�,@Z��@Zi�@Z&�@Y�3@Y\�@X�K@X�@X�I@X�I@X2�@W��@W��@W�w@W��@W�0@W��@W��@W��@W�@V҉@V�h@V��@V� @Vq�@VYK@VOv@V:*@U��@U�^@U��@U��@Uu�@U<6@T��@T��@T��@Tl"@S�]@Sݘ@S�K@S��@SZ�@S�@R��@R~�@RJ�@Q�D@Q�)@Q��@QrG@P��@P��@P�@P~@O�@OS�@O33@O�@N�@N��@N@M�^@M�-@M�-@M�S@MIR@L��@L�@L|�@L1'@K��@K�6@K��@Kt�@KF�@K@O@K,�@K&@K�@J�1@JOv@I�9@I��@I#�@H�f@H�K@H�@H֡@H��@G��@G&@F}V@E�o@ErG@E=�@E2a@E-w@E*0@E&�@E�@D��@D�.@D,=@C�;@C��@B�'@B?@A�z@A��@Aw2@@ѷ@@U2@@"h@@�@?�@?iD@?@O@?�@>�@>�@=�-@=j@=%F@<��@<g8@<�@;��@;��@;O@;Y@:��@:�]@:�s@:ȴ@:��@:�b@:�F@:��@:xl@:Ov@:)�@9�T@9�h@9e,@9;@8�Y@7��@7��@7��@7P�@76z@7@6�@6ȴ@6�m@6��@6xl@6s�@6c @6M�@68�@5��@5�@4��@4�@4�@4�Y@3��@3"�@2�A@2?@1�@1f�@0�5@0�@0x@0�@0�@/�Q@/t�@.��@.�x@.5?@.+k@.O@-�@-�X@-��@-a�@-F@-0�@-+@-V@-@-%@,�`@,֡@,��@,PH@+��@+��@+E9@+,�@+"�@*�y@*��@*{�@*@�@*J@)�@)�H@)�=@)��@)}�@)j@)2a@(�?@(��@(�9@(�$@(��@(��@(tT@(A�@(�@'� @'��@'J#@'C@'�@&��@&͟@&�!@&v�@&ff@&)�@%�@%�^@%k�@%!�@%;@$�@$�[@$Ĝ@$�j@$��@$Q�@$:�@$b@#�V@#@O@#'�@#C@#o@#
=@"�@"�@"�2@"��@"Q@"#:@!�@!�@!�X@!��@!�7@!c�@!Dg@ �P@ �u@ _@ I�@ ,=@ b@ �@�r@��@�K@X�@1�@C@�@҉@�@YK@8�@.�@
�@��@��@B�@��@��@`�@%�@�W@{J@E9@"�@o@�"@�h@�@��@ϫ@��@4@�	@�@��@�j@�@z�@j@PH@@�r@�m@�q@4�@�@��@�@��@n�@�@_@��@�@�-@B�@�@	l@;@�U@�@Z@�0@��@J#@�@��@��@��@n�@J�@�.@�9@rG@�@��@�_@bN@:�@خ@e�@U�@�@ߤ@�@5?@e@�@��@�@�@T�@B�@ \@	l@�@�O@�_@��@��@|�@g8@V�@M@N�@�@�Q@��@�{@a@O@/�@�@
�@
��@
��@
�\@
ff@
($@
 �@	ԕ@	@	��@	��@	|@	G�@	Dg@	7L@	(�@	%F@	 \@	�@�@�9@~(@Z@6@@�A@�w@�4@iD@C�@!-@�@�]@�R@��@� @8�@ �@��@��@��@�h@}�@Q�@+@�	@Ĝ@��@M@-�@-�@�@�@�4@~�@n/@RT@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BzB{Bz^Bz^Bz�Bz^Bz�Bz^Bz�Bz^BzxBy�Bx�BsMBG�B�B�B�LB��Bw�BRBNpBG�B3B*�B%�B�B9B�B�&B�B��BϑB�B�wB�kB�&B��B��B�B�B�vB��Bw2BffBN"BGzB2�BeB�B�+B��B�BӏBΥB��B�B�6B��B�dB}�BmwBb4BXBR BC�B:�B3�B+�B#nB�B�BB�B
��B
�6B
��B
��B
�qB
�B
��B
�,B
��B
�jB
�xB
��B
�TB
�'B
��B
��B
��B
�sB
�2B
��B
�$B
�B
�#B
��B
~�B
{�B
w�B
kQB
c�B
_VB
ZB
VSB
T�B
TFB
S[B
R:B
Q�B
O�B
J�B
H1B
C�B
@4B
<6B
6B
2-B
)_B
!�B
QB
�B
MB
 B
�B
�B
 �B	��B	��B	�+B	��B	�B	�*B	�LB	�TB	��B	��B	��B	׍B	յB	��B	��B	�B	��B	�~B	�uB	�B	�B	�XB	�B	��B	�2B	��B	��B	�B	��B	�B	�B	�=B	��B	�$B	��B	��B	�hB	��B	�B	�B	��B	�RB	�B	��B	� B	}"B	{�B	z�B	x�B	v�B	sB	l�B	g�B	d�B	a�B	^B	ZkB	T�B	IlB	FtB	A�B	=�B	1[B	+QB	&�B	!|B	�B	�B	�B	qB	�B	�B	FB	�B	�B	�B	�B	VB	0B		�B	KB	�B	�B	 �B�HB�"B�B�B��B��B��B�;B�B�B�)B�"B�B�*B��B�FB�&B��B�B�B�vB��B��B�IB�=B�kB�
B��B�2B�&B�TBуB�NB��BοB�pB͹B̘B�~BɺB�B�KB��B�zB�B�%B�%B�mB�B�gB�MB��BðB��B�GB�GB�3B��B�GB�{B�BÖB�-B�B�MB��BĜB�%B��B�YB�YB�YB�tB�B�tBżB��B��B��B�XB�#BɆB�rB��BϫB�pB�<B��B�6BңBңB�@B�aB�gB��B�B��B�)B��B��B��B�!B�-B��B�B�*B�eB�B�QB�B��B�5B��B�B�B�<B��B�}B	�B	B	aB	�B	�B	�B	BB	\B	�B	B	�B	�B	sB	 \B	 �B	$@B	%�B	&2B	'B	-�B	0;B	0;B	0�B	3hB	6�B	="B	=�B	>(B	?}B	D�B	E�B	E�B	H�B	K�B	O�B	PHB	R�B	WsB	XEB	ZB	Z�B	]/B	`B	hXB	k�B	ncB	o�B	rB	tB	w�B	{�B	}�B	~]B	�B	��B	�SB	�1B	�xB	�VB	��B	��B	�mB	�YB	��B	�B	�1B	��B	��B	�@B	��B	�6B	��B	��B	��B	��B	�B	��B	��B	�B	�2B	�8B	��B	�^B	�]B	�7B	��B	̈́B	͹B	�<B	οB	�\B	�\B	ѝB	ҽB	��B	�{B	�2B	՛B	��B	�KB	ںB	�WB	�~B	ޞB	�pB	ߤB	�vB	�NB	��B	�:B	��B	�FB	��B	�B	�!B	�AB	�GB	��B	��B	�xB	�<B	�wB
 B
�B
B
	�B
�B
jB
VB
vB
.B
�B
�B
�B
�B
CB
�B
�B
OB
�B
�B
 �B
!bB
# B
%�B
)B
*�B
+�B
,WB
,�B
-wB
-�B
.}B
/�B
0;B
1AB
3B
5tB
7�B
8�B
:B
;B
=�B
=�B
>(B
?B
?�B
@�B
AUB
B�B
F%B
E�B
GB
H1B
I7B
I�B
I�B
MB
P�B
R�B
SB
T�B
VmB
X_B
]�B
`BB
c�B
g�B
i*B
i�B
i�B
jeB
jB
j�B
mB
poB
q�B
sB
tB
t�B
u�B
vB
vzB
wB
x�B
y�B
z�B
{�B
|B
|�B
}<B
cB
��B
��B
��B
�tB
�YB
��B
�0B
��B
�PB
�B
�VB
�pB
��B
�vB
��B
�}B
�hB
� B
�[B
��B
��B
��B
��B
�
B
�EB
��B
��B
��B
��B
�B
�1B
�KB
�B
��B
��B
��B
��B
�dB
��B
�B
��B
�B
��B
��B
��B
�B
�\B
��B
��B
��B
��B
��B
��B
�,B
�FB
��B
��B
��B
��B
�DB
��B
��B
��B
�6B
�"B
�wB
��B
��B
��B
�[B
��B
��B
�|B
�|B
��B
�?B
��B
��B
��B
�+B
�B
��B
��B
��B
��B
�B
�xB
�B
�JB
��B
��B
��B
��B
�6B
�qB
��B
�.B
��B
��B
�oB
�oB
��B
��B
�AB
ÖB
�mB
��B
�1B
�7B
ɺB
ɺB
��B
��B
��B
��B
�=B
�B
��B
�~B
�B
οB
ϫB
��B
� B
�4B
҉B
�uB
��B
��B
�aB
�2B
�gB
��B
��B
��B
ؓB
��B
ٚB
ڠB
�	B
ۦB
��B
�xB
�IB
ݲB
��B
�B
�5B
�OB
�OB
ބB
޸B
޸B
��B
�!B
�VB
��B
��B
��B
�B
�B
�B
��B
�@B
�B
��B
�zB
�zB
��B
�B
��B
�LB
�fB
�B
�B
�B
�B
��B
��B
��B
��B
�B
�6B
��B
��B
�]B
��B
��B
�B
�oB
�AB
�'B
�AB
�AB
�-B
��B
�3B
�B
��B
��B
�nB
��B
��B
��B
�?B
�ZB
��B
��B
��B
��B
��B
��B
�`B
�zB
�LB
�B
�RB
��B
��B
��B
�$B
��B
��B
�DB
�^B
��B
��B
�B
�B
�B
�B
�PB
�PB
�jB
�jB
�jB
��B
��B
�VB
��B
�(B
�]B
�HB
��B
��B  B OB iB OB OB �B �B �BoB�BAB�B�B�BB�B�B�BMBB�B�B�B�B�B�B�BB?B�B_BzB�B�B�B�B�B�BfB	7B	lB	lB	�B	�B	�B	�B	�B
=B
�B
�B)BB�B�B�B�B�BB6BPBBpBpB�B�B\B�BHBbB}B}BBB:BoB�B@B�B�B�B�BFB{B�B�BMBgBMB�B9BBmB�B�B�BsB�B�BB�BKBKBB1BB�BB	B�B�BxB�B�B�BIBdB~BIBB�B�BpB�B�B �B!B �B!|B"B"�B# B#TB#TB#�B#nB#�B$B$B$@B$�B$�B%,B%�B%�B%�B%�B&B&2B&LB&B&fB&�B&�B'B'8B'mB'�B(
B(>B(>B(sB(�B(�B)DB)_B)�B)�B)�B)�B*0B*0B*B*0B*B*B*B*KB*B*�B*�B+B+6B+�B+�B+�B,B,B,WB,�B,�B-CB-]B-wB-wB.B.cB.�B.}B.}B.�B.�B/ B/iB/iB/�B0B0oB0�B0�B0�B1�B1�B1�B1�B1�B2aB2�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444BzB{Bz^Bz^Bz�Bz^Bz�Bz^Bz�Bz^BzxBy�Bx�BsMBG�B�B�B�LB��Bw�BRBNpBG�B3B*�B%�B�B9B�B�&B�B��BϑB�B�wB�kB�&B��B��B�B�B�vB��Bw2BffBN"BGzB2�BeB�B�+B��B�BӏBΥB��B�B�6B��B�dB}�BmwBb4BXBR BC�B:�B3�B+�B#nB�B�BB�B
��B
�6B
��B
��B
�qB
�B
��B
�,B
��B
�jB
�xB
��B
�TB
�'B
��B
��B
��B
�sB
�2B
��B
�$B
�B
�#B
��B
~�B
{�B
w�B
kQB
c�B
_VB
ZB
VSB
T�B
TFB
S[B
R:B
Q�B
O�B
J�B
H1B
C�B
@4B
<6B
6B
2-B
)_B
!�B
QB
�B
MB
 B
�B
�B
 �B	��B	��B	�+B	��B	�B	�*B	�LB	�TB	��B	��B	��B	׍B	յB	��B	��B	�B	��B	�~B	�uB	�B	�B	�XB	�B	��B	�2B	��B	��B	�B	��B	�B	�B	�=B	��B	�$B	��B	��B	�hB	��B	�B	�B	��B	�RB	�B	��B	� B	}"B	{�B	z�B	x�B	v�B	sB	l�B	g�B	d�B	a�B	^B	ZkB	T�B	IlB	FtB	A�B	=�B	1[B	+QB	&�B	!|B	�B	�B	�B	qB	�B	�B	FB	�B	�B	�B	�B	VB	0B		�B	KB	�B	�B	 �B�HB�"B�B�B��B��B��B�;B�B�B�)B�"B�B�*B��B�FB�&B��B�B�B�vB��B��B�IB�=B�kB�
B��B�2B�&B�TBуB�NB��BοB�pB͹B̘B�~BɺB�B�KB��B�zB�B�%B�%B�mB�B�gB�MB��BðB��B�GB�GB�3B��B�GB�{B�BÖB�-B�B�MB��BĜB�%B��B�YB�YB�YB�tB�B�tBżB��B��B��B�XB�#BɆB�rB��BϫB�pB�<B��B�6BңBңB�@B�aB�gB��B�B��B�)B��B��B��B�!B�-B��B�B�*B�eB�B�QB�B��B�5B��B�B�B�<B��B�}B	�B	B	aB	�B	�B	�B	BB	\B	�B	B	�B	�B	sB	 \B	 �B	$@B	%�B	&2B	'B	-�B	0;B	0;B	0�B	3hB	6�B	="B	=�B	>(B	?}B	D�B	E�B	E�B	H�B	K�B	O�B	PHB	R�B	WsB	XEB	ZB	Z�B	]/B	`B	hXB	k�B	ncB	o�B	rB	tB	w�B	{�B	}�B	~]B	�B	��B	�SB	�1B	�xB	�VB	��B	��B	�mB	�YB	��B	�B	�1B	��B	��B	�@B	��B	�6B	��B	��B	��B	��B	�B	��B	��B	�B	�2B	�8B	��B	�^B	�]B	�7B	��B	̈́B	͹B	�<B	οB	�\B	�\B	ѝB	ҽB	��B	�{B	�2B	՛B	��B	�KB	ںB	�WB	�~B	ޞB	�pB	ߤB	�vB	�NB	��B	�:B	��B	�FB	��B	�B	�!B	�AB	�GB	��B	��B	�xB	�<B	�wB
 B
�B
B
	�B
�B
jB
VB
vB
.B
�B
�B
�B
�B
CB
�B
�B
OB
�B
�B
 �B
!bB
# B
%�B
)B
*�B
+�B
,WB
,�B
-wB
-�B
.}B
/�B
0;B
1AB
3B
5tB
7�B
8�B
:B
;B
=�B
=�B
>(B
?B
?�B
@�B
AUB
B�B
F%B
E�B
GB
H1B
I7B
I�B
I�B
MB
P�B
R�B
SB
T�B
VmB
X_B
]�B
`BB
c�B
g�B
i*B
i�B
i�B
jeB
jB
j�B
mB
poB
q�B
sB
tB
t�B
u�B
vB
vzB
wB
x�B
y�B
z�B
{�B
|B
|�B
}<B
cB
��B
��B
��B
�tB
�YB
��B
�0B
��B
�PB
�B
�VB
�pB
��B
�vB
��B
�}B
�hB
� B
�[B
��B
��B
��B
��B
�
B
�EB
��B
��B
��B
��B
�B
�1B
�KB
�B
��B
��B
��B
��B
�dB
��B
�B
��B
�B
��B
��B
��B
�B
�\B
��B
��B
��B
��B
��B
��B
�,B
�FB
��B
��B
��B
��B
�DB
��B
��B
��B
�6B
�"B
�wB
��B
��B
��B
�[B
��B
��B
�|B
�|B
��B
�?B
��B
��B
��B
�+B
�B
��B
��B
��B
��B
�B
�xB
�B
�JB
��B
��B
��B
��B
�6B
�qB
��B
�.B
��B
��B
�oB
�oB
��B
��B
�AB
ÖB
�mB
��B
�1B
�7B
ɺB
ɺB
��B
��B
��B
��B
�=B
�B
��B
�~B
�B
οB
ϫB
��B
� B
�4B
҉B
�uB
��B
��B
�aB
�2B
�gB
��B
��B
��B
ؓB
��B
ٚB
ڠB
�	B
ۦB
��B
�xB
�IB
ݲB
��B
�B
�5B
�OB
�OB
ބB
޸B
޸B
��B
�!B
�VB
��B
��B
��B
�B
�B
�B
��B
�@B
�B
��B
�zB
�zB
��B
�B
��B
�LB
�fB
�B
�B
�B
�B
��B
��B
��B
��B
�B
�6B
��B
��B
�]B
��B
��B
�B
�oB
�AB
�'B
�AB
�AB
�-B
��B
�3B
�B
��B
��B
�nB
��B
��B
��B
�?B
�ZB
��B
��B
��B
��B
��B
��B
�`B
�zB
�LB
�B
�RB
��B
��B
��B
�$B
��B
��B
�DB
�^B
��B
��B
�B
�B
�B
�B
�PB
�PB
�jB
�jB
�jB
��B
��B
�VB
��B
�(B
�]B
�HB
��B
��B  B OB iB OB OB �B �B �BoB�BAB�B�B�BB�B�B�BMBB�B�B�B�B�B�B�BB?B�B_BzB�B�B�B�B�B�BfB	7B	lB	lB	�B	�B	�B	�B	�B
=B
�B
�B)BB�B�B�B�B�BB6BPBBpBpB�B�B\B�BHBbB}B}BBB:BoB�B@B�B�B�B�BFB{B�B�BMBgBMB�B9BBmB�B�B�BsB�B�BB�BKBKBB1BB�BB	B�B�BxB�B�B�BIBdB~BIBB�B�BpB�B�B �B!B �B!|B"B"�B# B#TB#TB#�B#nB#�B$B$B$@B$�B$�B%,B%�B%�B%�B%�B&B&2B&LB&B&fB&�B&�B'B'8B'mB'�B(
B(>B(>B(sB(�B(�B)DB)_B)�B)�B)�B)�B*0B*0B*B*0B*B*B*B*KB*B*�B*�B+B+6B+�B+�B+�B,B,B,WB,�B,�B-CB-]B-wB-wB.B.cB.�B.}B.}B.�B.�B/ B/iB/iB/�B0B0oB0�B0�B0�B1�B1�B1�B1�B1�B2aB2�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230731005752  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230731005754  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230731005755  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230731005755                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230731005756  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230731005756  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230731010937                      G�O�G�O�G�O�                