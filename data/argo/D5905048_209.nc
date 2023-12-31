CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-10T00:35:32Z creation;2018-02-10T00:35:40Z conversion to V3.1;2019-12-19T07:45:33Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180210003532  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_209                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�K5�f�1   @�K6�l @3h�\)�dY�7Kƨ1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@#�
@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��HB�
B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDTw
DT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D��D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�;�D�~�D羸D��D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��A�A�-A�ZA�v�A�v�A�p�A�XA�=qA��A��A���AɮA�p�A�  Aț�A�XA�"�AǼjA�M�A��A��#A�7LA�  AŶFA�jA�I�A�A�A�=qA�9XA��A�+A���A���A��A�v�A�+A��;A�dZA�%A���A���A��wA���A�dZA��7A�/A��A��HA�\)A��A�(�A�  A��!A�?}A�M�A�jA��RA�l�A���A���A�  A���A���A�(�A�%A���A��;A�A���A�\)A���A�VA���A��A���A��!A�
=A�`BA��`A��A��
A�Q�A�"�A��TA��uA�O�A��-A��A�&�A���A�A�&�A�`BA~��A}A{"�Ay�^Ay;dAx��Aw��Av�As��Aq�ApĜAnJAl��Ah9XAbr�Aa`BA`�jA`ffA_�FA_oA]��A[�AX�AW��AU��ATZAR��AQ�TAQ�AO33ANI�AN �AMx�AK��AIt�AH$�AG`BAF�AE�AD-ABjAA��AA�A?��A>�`A<�HA<VA<JA;�hA:�A:VA8��A6�A6�DA3��A1��A/��A.�uA.�A-x�A+�^A*A�A)dZA(bNA'�
A'�A&�RA%��A$��A$$�A#�TA#\)A"�uA"-A!G�A ��A��A�Az�A��A�AM�A�!A�A�!A�mAAE�A�hAI�Az�AXAE�A��AA
ȴA	�-A	+A�Az�A�;A�AjAl�AA��A�
A/A �DA Q�@��@��-@�$�@�z�@�dZ@���@�b@�p�@�j@@���@�1@���@�O�@�"�@���@�hs@�9@�`B@�33@�^5@ݩ�@ܓu@���@�$�@ى7@���@�9X@��H@�%@�A�@��;@Ӯ@��@�E�@�x�@��@Л�@ϝ�@�C�@�
=@ΰ!@�/@��m@���@�{@ȋD@Ǯ@Ƨ�@��T@�`B@��`@�Q�@�|�@¸R@�=q@���@��@�|�@�
=@�n�@�@�X@�9X@�33@���@��+@���@��/@���@�Z@�;d@�ȴ@�E�@���@��#@��@�@�ff@���@���@�Ĝ@�C�@���@��T@���@�?}@�V@�V@��j@�j@��D@��D@�9X@� �@�ƨ@�|�@��@�v�@�ff@�=q@���@���@�  @��P@��@�ff@�$�@��7@�?}@�V@���@�Ĝ@�j@��@�t�@�5?@��7@�O�@���@�j@� �@���@�\)@�;d@���@���@�l�@�dZ@�dZ@�K�@�;d@�33@��@���@���@��\@��+@��+@�v�@�^5@�=q@�$�@���@�@��7@�/@��/@���@�1'@��@�|�@��@�K�@��@�\)@���@�@���@�`B@��@��/@���@�?}@���@��9@�(�@��@��@���@�ȴ@���@���@�v�@�M�@���@�p�@��@���@�r�@�I�@�1'@�1@��m@��P@��@���@��!@���@��\@�5?@�$�@�{@���@���@���@��@�X@�&�@���@��j@���@�j@�1'@�b@���@��m@���@���@��@���@���@�l�@��@���@��@��y@��y@���@�^5@�V@�=q@�J@��-@��@�7L@�r�@���@��
@���@��F@�l�@���@�v�@�n�@�V@�M�@�=q@��@��^@���@��h@�%@���@��D@�j@�b@���@��@��m@��;@��F@�;d@�@��R@�~�@��@���@�p�@�7L@���@���@���@�Z@�1'@�1@��
@��F@�|�@�\)@��y@�n�@���@���@�O�@���@�r�@��@��u@���@��9@��9@���@�A�@��;@�ƨ@��@��@�+@��R@��!@�M�@��@�hs@���@���@��9@�z�@�1'@�w@;d@
=@}�T@|��@|�@}/@}�@|�D@{�F@{dZ@{C�@{"�@z�@z��@z��@z��@zn�@y�@yX@x�u@x�@w�w@vȴ@u��@uO�@u?}@t��@t�@tZ@t1@s�m@s�F@r�!@r��@r�\@rn�@r�@q��@q��@q��@q�7@qX@q�@p�9@p�u@pr�@pQ�@p �@o�P@o
=@m�-@m?}@m/@m�@m�@l��@l��@l�/@l��@l�j@k��@kdZ@j�H@j^5@i��@i&�@h1'@g;d@f�+@f5?@e��@e`B@d��@d��@dz�@dj@dI�@d9X@c�m@c��@cdZ@cC�@b�H@bM�@b-@b-@bJ@a��@`�`@`Q�@`b@`  @_�@_�;@_�@_l�@_;d@_+@_
=@^��@^��@^@]`B@\��@\9X@[�
@[��@[t�@[C�@[@Z�\@Zn�@ZJ@Y�#@Y7L@X��@X1'@W�w@W|�@W;d@Vȴ@Vv�@VE�@U@Up�@T�/@T�@Tz�@TZ@T(�@Sƨ@S��@St�@S"�@R�!@RM�@RJ@Q�^@Qhs@P�`@PQ�@Pb@O�@O��@O�@O�P@O|�@Ol�@O;d@N��@N�y@N�@N��@N�+@NV@N{@M��@M�@L�@Lj@L�@K�@J�@J�H@J^5@J�@I�@I�#@I�^@I��@I��@Ix�@Ix�@IX@H�`@H��@H�@H  @G�w@G|�@F�R@FV@F{@E@E�@D��@DZ@D(�@C�m@Cƨ@C��@B�@B��@B�\@B~�@B=q@BJ@A�#@A��@A��@A�7@A�7@Ahs@AX@AG�@AG�@AG�@A7L@@�`@@ �@?�P@>�y@>�+@>E�@>5?@>{@>@=�T@=�-@=�h@=p�@=`B@=?}@=�@<��@<��@<�@<��@<j@<Z@<(�@<1@;�m@;��@:��@:n�@:�@9�@9��@9X@8A�@7�@7��@7�P@7;d@6v�@6V@6V@6E�@65?@6$�@5�@4��@4��@4z�@4I�@4�@3�
@3��@2�@1��@1x�@0�9@0r�@0b@/�;@/�@/l�@/K�@/+@.��@.�R@.�R@.��@.@-@,�j@,�D@,z�@,j@,Z@,I�@,(�@,�@,�@+�
@+dZ@+33@*��@*�@)��@)�@)��@)��@)x�@)�@(��@(�@(A�@'�@'�w@'��@'�P@'|�@'\)@'K�@'�@&�R@&v�@&5?@&$�@%�T@%�-@%�@%p�@%O�@%V@%V@%V@$�@$�/@$��@$�@$�D@$Z@$9X@$9X@$(�@#�F@#�@#dZ@#S�@#33@#o@#@"^5@"=q@"-@"�@!��@!�@!��@!��@!X@!�@ Ĝ@ ��@ Q�@  �@  �@ b@ b@�@�w@��@|�@|�@l�@
=@��@V@@��@�@p�@`B@`B@O�@?}@�@�@Z@Z@Z@9X@1@�@33@@��@�@hs@7L@&�@%@��@bN@b@�@;d@
=@��@V@{@�T@��@O�@�j@j@j@j@j@z�@z�@z�@z�@z�@z�@I�@��@�
@dZ@S�@C�@33@33@33@33@"�@o@�@��@~�@M�@�@�@�@�@�@�#@�7@�7@hs@��@Q�@Q�@b@�@|�@l�@;d@�@�y@�@�R@��@��@V@��@`B@O�@O�@?}@?}@/@�@V@��@�@�/@�/@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��A�A�-A�ZA�v�A�v�A�p�A�XA�=qA��A��A���AɮA�p�A�  Aț�A�XA�"�AǼjA�M�A��A��#A�7LA�  AŶFA�jA�I�A�A�A�=qA�9XA��A�+A���A���A��A�v�A�+A��;A�dZA�%A���A���A��wA���A�dZA��7A�/A��A��HA�\)A��A�(�A�  A��!A�?}A�M�A�jA��RA�l�A���A���A�  A���A���A�(�A�%A���A��;A�A���A�\)A���A�VA���A��A���A��!A�
=A�`BA��`A��A��
A�Q�A�"�A��TA��uA�O�A��-A��A�&�A���A�A�&�A�`BA~��A}A{"�Ay�^Ay;dAx��Aw��Av�As��Aq�ApĜAnJAl��Ah9XAbr�Aa`BA`�jA`ffA_�FA_oA]��A[�AX�AW��AU��ATZAR��AQ�TAQ�AO33ANI�AN �AMx�AK��AIt�AH$�AG`BAF�AE�AD-ABjAA��AA�A?��A>�`A<�HA<VA<JA;�hA:�A:VA8��A6�A6�DA3��A1��A/��A.�uA.�A-x�A+�^A*A�A)dZA(bNA'�
A'�A&�RA%��A$��A$$�A#�TA#\)A"�uA"-A!G�A ��A��A�Az�A��A�AM�A�!A�A�!A�mAAE�A�hAI�Az�AXAE�A��AA
ȴA	�-A	+A�Az�A�;A�AjAl�AA��A�
A/A �DA Q�@��@��-@�$�@�z�@�dZ@���@�b@�p�@�j@@���@�1@���@�O�@�"�@���@�hs@�9@�`B@�33@�^5@ݩ�@ܓu@���@�$�@ى7@���@�9X@��H@�%@�A�@��;@Ӯ@��@�E�@�x�@��@Л�@ϝ�@�C�@�
=@ΰ!@�/@��m@���@�{@ȋD@Ǯ@Ƨ�@��T@�`B@��`@�Q�@�|�@¸R@�=q@���@��@�|�@�
=@�n�@�@�X@�9X@�33@���@��+@���@��/@���@�Z@�;d@�ȴ@�E�@���@��#@��@�@�ff@���@���@�Ĝ@�C�@���@��T@���@�?}@�V@�V@��j@�j@��D@��D@�9X@� �@�ƨ@�|�@��@�v�@�ff@�=q@���@���@�  @��P@��@�ff@�$�@��7@�?}@�V@���@�Ĝ@�j@��@�t�@�5?@��7@�O�@���@�j@� �@���@�\)@�;d@���@���@�l�@�dZ@�dZ@�K�@�;d@�33@��@���@���@��\@��+@��+@�v�@�^5@�=q@�$�@���@�@��7@�/@��/@���@�1'@��@�|�@��@�K�@��@�\)@���@�@���@�`B@��@��/@���@�?}@���@��9@�(�@��@��@���@�ȴ@���@���@�v�@�M�@���@�p�@��@���@�r�@�I�@�1'@�1@��m@��P@��@���@��!@���@��\@�5?@�$�@�{@���@���@���@��@�X@�&�@���@��j@���@�j@�1'@�b@���@��m@���@���@��@���@���@�l�@��@���@��@��y@��y@���@�^5@�V@�=q@�J@��-@��@�7L@�r�@���@��
@���@��F@�l�@���@�v�@�n�@�V@�M�@�=q@��@��^@���@��h@�%@���@��D@�j@�b@���@��@��m@��;@��F@�;d@�@��R@�~�@��@���@�p�@�7L@���@���@���@�Z@�1'@�1@��
@��F@�|�@�\)@��y@�n�@���@���@�O�@���@�r�@��@��u@���@��9@��9@���@�A�@��;@�ƨ@��@��@�+@��R@��!@�M�@��@�hs@���@���@��9@�z�@�1'@�w@;d@
=@}�T@|��@|�@}/@}�@|�D@{�F@{dZ@{C�@{"�@z�@z��@z��@z��@zn�@y�@yX@x�u@x�@w�w@vȴ@u��@uO�@u?}@t��@t�@tZ@t1@s�m@s�F@r�!@r��@r�\@rn�@r�@q��@q��@q��@q�7@qX@q�@p�9@p�u@pr�@pQ�@p �@o�P@o
=@m�-@m?}@m/@m�@m�@l��@l��@l�/@l��@l�j@k��@kdZ@j�H@j^5@i��@i&�@h1'@g;d@f�+@f5?@e��@e`B@d��@d��@dz�@dj@dI�@d9X@c�m@c��@cdZ@cC�@b�H@bM�@b-@b-@bJ@a��@`�`@`Q�@`b@`  @_�@_�;@_�@_l�@_;d@_+@_
=@^��@^��@^@]`B@\��@\9X@[�
@[��@[t�@[C�@[@Z�\@Zn�@ZJ@Y�#@Y7L@X��@X1'@W�w@W|�@W;d@Vȴ@Vv�@VE�@U@Up�@T�/@T�@Tz�@TZ@T(�@Sƨ@S��@St�@S"�@R�!@RM�@RJ@Q�^@Qhs@P�`@PQ�@Pb@O�@O��@O�@O�P@O|�@Ol�@O;d@N��@N�y@N�@N��@N�+@NV@N{@M��@M�@L�@Lj@L�@K�@J�@J�H@J^5@J�@I�@I�#@I�^@I��@I��@Ix�@Ix�@IX@H�`@H��@H�@H  @G�w@G|�@F�R@FV@F{@E@E�@D��@DZ@D(�@C�m@Cƨ@C��@B�@B��@B�\@B~�@B=q@BJ@A�#@A��@A��@A�7@A�7@Ahs@AX@AG�@AG�@AG�@A7L@@�`@@ �@?�P@>�y@>�+@>E�@>5?@>{@>@=�T@=�-@=�h@=p�@=`B@=?}@=�@<��@<��@<�@<��@<j@<Z@<(�@<1@;�m@;��@:��@:n�@:�@9�@9��@9X@8A�@7�@7��@7�P@7;d@6v�@6V@6V@6E�@65?@6$�@5�@4��@4��@4z�@4I�@4�@3�
@3��@2�@1��@1x�@0�9@0r�@0b@/�;@/�@/l�@/K�@/+@.��@.�R@.�R@.��@.@-@,�j@,�D@,z�@,j@,Z@,I�@,(�@,�@,�@+�
@+dZ@+33@*��@*�@)��@)�@)��@)��@)x�@)�@(��@(�@(A�@'�@'�w@'��@'�P@'|�@'\)@'K�@'�@&�R@&v�@&5?@&$�@%�T@%�-@%�@%p�@%O�@%V@%V@%V@$�@$�/@$��@$�@$�D@$Z@$9X@$9X@$(�@#�F@#�@#dZ@#S�@#33@#o@#@"^5@"=q@"-@"�@!��@!�@!��@!��@!X@!�@ Ĝ@ ��@ Q�@  �@  �@ b@ b@�@�w@��@|�@|�@l�@
=@��@V@@��@�@p�@`B@`B@O�@?}@�@�@Z@Z@Z@9X@1@�@33@@��@�@hs@7L@&�@%@��@bN@b@�@;d@
=@��@V@{@�T@��@O�@�j@j@j@j@j@z�@z�@z�@z�@z�@z�@I�@��@�
@dZ@S�@C�@33@33@33@33@"�@o@�@��@~�@M�@�@�@�@�@�@�#@�7@�7@hs@��@Q�@Q�@b@�@|�@l�@;d@�@�y@�@�R@��@��@V@��@`B@O�@O�@?}@?}@/@�@V@��@�@�/@�/@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�RB
��B
��B�B�B�B'�B-B0!BA�BQ�Bt�B�=B��B�B�qBÖB��B�HB�fB�HB��B��B��B  B��B�B�B��B�PB�B��B�JB�dB�B��B��B��B�B�B�!B��Bx�BP�B�\B�NB��BDB!�B)�B�B�B�BDBBuB�BJB��B��B�`B��B�)B�`B�`B�BB�B��B�dB�VBL�BJ�BA�BG�B8RB�B�BbB1B
�B
�'B
��B
��B
��B
r�B
� B
r�B
S�B
]/B
ZB
C�B
=qB
1'B
#�B
 �B
�B
�B
�B
1B	��B	�HB	��B	�
B	�RB	�B	�=B	K�B	�B	�B	�B	z�B	p�B	`BB	H�B	>wB	E�B	7LB	6FB	/B	.B	(�B	�B	�B	�B	uB	  B�B�B��B�B�sB�
B��B�B��BɺB��B�^B��BƨB��B�^B�3B��B��B��B�7Bx�B�B�7B�uB�1B{�By�B�B�B�%B�%B}�Bx�By�B~�B�B|�Bu�Bx�Br�Bq�Bl�Bn�Bm�BdZB_;BF�BQ�BS�BffB`BBYBZBT�BJ�B>wBK�BK�BI�BM�BR�BK�BVBJ�BK�BT�BW
BS�BS�BYBZBQ�BW
BVB\)BT�BT�BF�BP�BW
BO�BN�BE�BQ�BP�BH�BG�BK�BL�BF�BQ�BW
BQ�BC�BL�B[#B\)BZBVB`BBbNB`BB_;B\)B\)BiyBm�Bn�BjBk�Bl�Br�Br�Bp�Bv�Bw�Bs�Bl�Bp�Bw�By�By�B� B�B�7B�VB�VB�VB�VB�hB��B�bB��B��B��B��B��B��B��B��B�-B�FB�-B�RB�wB�}B�jBƨB��B��B��B�B�/B�HB�BB�HB�;B�;B�B�B��B��B��B	  B��B	B	
=B	JB	PB	hB	hB	oB	hB	�B	�B	�B	�B	�B	 �B	'�B	'�B	.B	2-B	7LB	:^B	>wB	?}B	D�B	E�B	F�B	G�B	F�B	Q�B	[#B	\)B	]/B	`BB	bNB	iyB	m�B	x�B	y�B	}�B	�B	�B	�B	�%B	�1B	�+B	�1B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�RB	�RB	�FB	�3B	�!B	�RB	�RB	�LB	�dB	��B	ƨB	ŢB	ĜB	ŢB	ƨB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�)B	�/B	�)B	�)B	�BB	�NB	�TB	�ZB	�TB	�fB	�`B	�ZB	�`B	�`B	�`B	�`B	�ZB	�`B	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
%B
%B
%B
B
B
%B
B
%B
%B
%B
1B
	7B

=B
JB
JB
DB
PB
PB
PB
VB
PB
VB
JB
JB
PB
VB
JB

=B
PB
bB
bB
hB
hB
oB
oB
hB
oB
�B
�B
�B
{B
{B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
 �B
$�B
$�B
$�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
'�B
'�B
%�B
%�B
#�B
'�B
+B
+B
+B
)�B
)�B
)�B
)�B
(�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
(�B
+B
,B
,B
,B
-B
/B
/B
/B
.B
.B
.B
.B
/B
.B
-B
0!B
0!B
/B
.B
-B
.B
0!B
1'B
1'B
1'B
1'B
0!B
1'B
1'B
1'B
0!B
/B
.B
/B
/B
0!B
2-B
33B
33B
33B
2-B
2-B
33B
33B
33B
2-B
2-B
49B
49B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
8RB
9XB
9XB
9XB
8RB
9XB
9XB
9XB
8RB
9XB
:^B
9XB
9XB
9XB
9XB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
=qB
=qB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
A�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
A�B
C�B
D�B
C�B
D�B
B�B
E�B
F�B
F�B
F�B
F�B
E�B
F�B
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
J�B
J�B
I�B
I�B
G�B
F�B
G�B
H�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
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
M�B
M�B
M�B
L�B
K�B
J�B
M�B
M�B
N�B
N�B
M�B
K�B
O�B
P�B
O�B
O�B
N�B
R�B
R�B
R�B
R�B
Q�B
O�B
P�B
S�B
S�B
S�B
S�B
R�B
Q�B
O�B
S�B
VB
S�B
W
B
W
B
XB
XB
XB
YB
YB
XB
YB
YB
XB
W
B
XB
VB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
ZB
[#B
ZB
[#B
]/B
]/B
]/B
]/B
]/B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
_;B
`BB
`BB
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
bNB
bNB
cTB
cTB
bNB
aHB
bNB
dZB
cTB
cTB
cTB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
ffB
e`B
e`B
ffB
gmB
gmB
gmB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
hsB
iyB
iyB
iyB
hsB
gmB
iyB
iyB
iyB
hsB
iyB
l�B
m�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
p�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
p�B
p�B
q�B
p�B
r�B
r�B
r�B
s�B
r�B
s�B
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
r�B
r�B
s�B
s�B
q�B
s�B
t�B
t�B
s�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
u�B
t�B
v�B
w�B
w�B
x�B
x�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
ѷB
��BMB�BB($B-]B0�BA�BR�Bu?B�)B��B��B��BāB͹B��B�B�B�%B�lB�wB OB�qB�G�O�B�B�}B��B��B��B�VB��B��B��B��B��B�OB�nB�zB}�BT{B�"B�-B�xB
�B#B,"B"�B�BB�B�B�B�B<B�B��B�>B�HB��B�B�zB�B��B��G�O�G�O�BT{BO�BEmBJ	B;JBB7B B
�G�O�G�O�B
�4B
�B
��B
w�B
��B
t�B
W�B
^�B
[�B
E�B
?}B
3�B
%�B
"�B
1B
OB
mB
	�B	��B	�@B	��B	�1G�O�B	��G�O�B	RTB	�9B	��B	��B	{�B	q�B	bhB	K�B	A;B	G_B	9�B	8B	0�B	/OB	*KB	�B	�B	!B	�B	uB�?B�ZB��B�B�B�KB�B�B�B�xB�B��B�AB�+B�[B�dB�nB�RB��B��G�O�B|B��B��B�B��B~(B{�B�-B�[B��B��B.BzDB{0B�B��B}�Bv�By�Bs�Br�Bm�Bo�Bn}Be�Ba|G�O�BTFBU�BgBabBZkB[=BVSBL�BABMPBM�BK�BOBBTaBMjBW
BL�BM�BU�BX+BU2BUMBY�BZ�BS&BW�BV�B\�BV9BVmBIBRBXBQ4BPHBGzBR�BQ�BJ	BIBL�BM�BHBR�BW�BR�BE�BN<B[�B\�B[	BW$B`�Bb�B`�B_�B]dB]�Bi�Bm�Bn�Bk6Bl"Bm)BsBs3Bq[BwBxBtTBm�Bq�Bx�Bz�Bz�B��B��B��B��B��B��B�B� B�B��B�B�BB�:B�tB�LB�zB��B��B�|B��B��B��B��B��B�VB�B�)B�.B�B�B�/B�-B�B�B�B�BB��B�AB�B�8B�6B	 B�HB	aB	
XB	dB	�B	�B	�B	�B	B	�B	�B	B	B	_B	!HB	(XB	(sB	.cB	2|B	7�B	:�B	>�B	?�B	D�B	E�B	GB	H1B	G�B	RoB	[WB	\xB	]�B	`�B	b�B	i�B	m�B	x�B	y�B	~B	�B	�'B	�-B	�?B	�KB	�_B	�fB	�\B	��B	��B	��B	��B	��B	��B	��B	�B	�>B	�DB	�eB	�WB	�IB	�]B	�cB	�iB	�cB	�B	�RB	�zB	��B	��B	�lB	��B	��B	�dB	�iB	ƨB	��B	�B	�%B	��B	��B	�=B	�B	�B	�B	�B	� B	�TB	�:B	�aB	�KB	�QB	�CB	�IB	�]B	�dB	�xB	�xB	�vB	�hB	�B	�tB	�B	�fB	�zB	�B	�zB	�zB	�zB	�zB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�-B	��B	��B	�	B	��B	�B	�tB	�"B	�B	�.B	�.B	�B	�BB	�<B	�B
 4B	�qB	�(B
'B
GB
[B
SB
%B
?B
YB
MB
[B
tB
�B
YB
tB
tB
�B
	�B

�B
~B
~B
xB
jB
jB
�B
pB
�B
�B
�B
�B
�B
�B
�B

�B
�B
HB
bB
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�G�O�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�G�O�B
�B
B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
!B
$�B
%B
$�B
$B
$�B
%�B
&�B
'B
'B
'B
'B
(
B
)B
(
B
($G�O�B
&2B
$@B
(
B
+B
+B
+B
*B
)�B
*0B
*B
)DG�O�B
($B
($B
($B
($B
'8B
'mB
'RB
)DB
+B
,=B
,=B
,=B
-)B
/5B
/5B
/OB
./B
./B
./B
./B
/5B
.IB
-CB
0;B
0;B
/OB
.IB
-]B
.IB
0;B
1AB
1'B
1AB
1AB
0;B
1[B
1[B
1AB
0UB
/OB
.IB
/iB
/OB
0UB
2GB
3MB
3MB
3MB
2GB
2aB
3MB
3hB
3MB
2aB
2aB
4nB
4nB
5ZB
5ZB
5�B
5ZB
6`B
5tB
6`B
6�B
8�B
9rB
9rB
9�B
8�B
9rB
9rB
9rB
8�B
9rB
:xB
9rB
9�B
9�B
9�B
<�B
<�B
=�B
=�B
=�B
=qB
=�B
=�B
=�B
>�B
>wB
=�B
=�B
=�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
A�B
C�B
D�B
C�B
D�G�O�B
E�B
F�B
F�B
F�B
F�B
E�B
F�B
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
J�B
J�B
I�B
I�G�O�B
F�B
G�B
H�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
L�B
M�B
NB
M�B
M�B
NB
NB
L�B
K�B
KB
NB
M�B
N�B
N�B
N"G�O�B
O�B
Q B
O�B
P.B
O(B
R�B
SB
SB
SB
R:G�O�B
QNB
TB
TB
TB
TB
S@B
RTG�O�B
T,B
VSG�O�B
W$B
W$B
X+B
X+B
X+B
Y1B
YKB
X+B
Y1B
Y1B
XEB
WYB
X_G�O�B
[=B
[=B
\CB
\)B
\CB
\CB
\)B
\)B
[=B
ZQB
[WB
ZQB
[WB
]IB
]/B
]IB
]dB
]IB
\]B
]dB
^OB
^OB
^OB
_VB
_VB
_;B
_;B
_pB
_VB
_VB
^�B
_pB
`\B
`\B
`\B
`\B
abB
abB
a|B
abB
bNB
bNB
bhB
bNB
bNB
bhB
b�B
bhB
cTB
cTB
bhB
a|B
b�B
dtB
cnB
cnB
c�B
cnB
b�B
dtB
ezB
e`B
e�B
e`B
dtB
d�B
dtB
dtB
d�B
ezB
ezB
f�B
gmB
gmB
gmB
f�B
f�B
f�B
f�B
g�B
f�B
e�B
e�B
f�B
g�B
g�B
g�B
i�B
iyB
iyB
iyB
iyB
h�B
g�B
h�B
iyB
i�B
i�B
h�B
g�B
i�B
i�B
i�B
h�B
i�B
l�B
m�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
p�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
p�B
p�B
q�B
p�B
r�B
r�B
r�B
s�B
r�B
s�B
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
r�B
r�B
s�B
s�G�O�B
s�B
t�B
t�B
s�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
u�B
t�B
v�B
w�B
w�B
x�B
x�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�1111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111441111111114411111111111111111111114141111111111111111111111111111111111111111411111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111114111141111411111111411114111111111111111111111111114111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111411111111111111111111111111111114111111111141111111411411111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
G�O�<#�
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
G�O�<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802140034172018021400341720180214003417201806221326062018062213260620180622132606201804050729442018040507294420180405072944  JA  ARFMdecpA19c                                                                20180210093526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180210003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180210003535  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180210003536  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180210003536  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180210003536  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180210003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180210003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180210003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180210003540                      G�O�G�O�G�O�                JA  ARUP                                                                        20180210005659                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180210153309  CV  JULD            G�O�G�O�F�Y�                JM  ARSQJMQC2.0                                                                 20180213000000  CF  PSAL_ADJUSTED_QCB�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180213153417  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180213153417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222944  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                