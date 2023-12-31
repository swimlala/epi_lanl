CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-05T00:36:01Z creation;2017-08-05T00:36:05Z conversion to V3.1;2019-12-19T08:00:50Z update;     
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20170805003601  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_146                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�����1   @��q��@3��䎊r�d��	�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�<�Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C(\C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�
D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�
D'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDTw
DT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�;�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�A�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ƨA���A���A���A�ĜA�Aܺ^Aܝ�A܁A�z�A�n�A�Q�A�?}A�7LA�1'A�$�A۴9A�VAٮA�A��HA��A�XA�Q�AɑhA�t�Aǰ!AƋDAŸRA���AÉ7A�p�A��wA��A��-A�K�A��A���A���A�ĜA��;A�`BA�E�A�G�A�JA�r�A���A���A��RA��;A�(�A���A��A��A�E�A��yA�x�A�7LA�JA���A��TA��yA�v�A��A�A�A��yA���A��A�ĜA�bA��yA�p�A�`BA���A�bA���A�7LA��PA�A��A��9A�1A��hA�%A�v�A�5?A�A�A��A��A�{A�ȴA�|�A��A��jA���A�  A�;dA��A��A�O�A�|�A��^A�VA��A�dZA~ȴA}�A{��Aw�;As��Aq�Ap5?Ap  AmƨAj�/Ah�Ag�mAgXAdjA`�A^�A^bA\�A[�AZA�AX�DAV(�AS&�AQ%AO��AN��ANAL�+AK|�AI�AHffAGK�AF�AFA�AE��AEG�AD�jAC��AC/AB��AB�AAdZA@-A>�A=��A<A�A: �A8��A6r�A5O�A4��A2^5A1��A0�DA/`BA-ƨA-/A-%A+��A*ZA)C�A(�A(�A&��A%ƨA%�A$��A#p�A"�+A!��A!;dA �A �DA (�A�^AO�A�A�A��A�A�mA"�A�\Al�A��A�wA��AdZA�AffA��A�A��A33An�A�wAdZA;dA��A��A�^A7LA9XA�mAO�A+AA�AO�A
�DA	��A	33A�A�jAdZA`BA �`A ĜA �!A ��A �+@���@���@�
=@�M�@�=q@���@��P@��@��h@���@��/@�dZ@�$�@�Ĝ@�C�@�o@�&�@���@��@�G�@���@��m@�;d@��@�\)@�l�@��y@��@�/@އ+@�-@�bN@�v�@٩�@�7L@��m@ְ!@���@ԃ@�C�@�@�hs@�V@��@ѩ�@�1'@�1'@Ͼw@Η�@�5?@͑h@���@�C�@�$�@ȓu@ư!@��T@�7L@���@���@���@�~�@��@�Q�@���@�v�@�hs@���@�A�@���@�K�@���@��!@���@�ff@��T@�A�@��
@��m@�  @�b@� �@���@��
@��w@��P@�t�@�^5@�O�@��@���@�K�@���@���@��P@�ȴ@�5?@��-@�?}@��@�V@��D@�ƨ@��@���@�S�@��+@��#@��7@�p�@�?}@���@�Ĝ@��9@��9@���@�I�@�  @��F@�t�@�t�@�t�@�\)@�K�@�;d@�+@���@��@���@��\@�@��-@��h@�x�@�hs@�hs@�O�@��@���@�I�@���@���@�t�@�;d@��@���@�M�@�=q@�5?@�$�@��@�J@��@���@�`B@�&�@��/@��u@�Q�@�b@���@���@�dZ@�S�@�C�@�33@�"�@�o@�
=@��@��H@���@�^5@���@���@�/@���@��9@�bN@� �@�b@��;@�t�@�33@�
=@��R@�v�@�-@���@��T@�`B@�V@��j@��@�z�@��u@��u@�Z@�1'@��
@�\)@�K�@�o@��@��H@���@��@��^@���@�p�@�`B@�&�@���@��@���@��@��m@���@��@�dZ@��y@���@�~�@�ff@�$�@�J@���@��^@���@���@�hs@�7L@�%@���@�b@��F@��@�S�@�+@�ȴ@���@�V@��@�@��h@�hs@�?}@�/@��`@��@��D@�Q�@�1'@�b@��@���@�l�@�S�@�C�@��@���@��@�ȴ@��!@�v�@�V@�{@���@��#@���@��^@���@�p�@�?}@���@��9@���@��D@�j@�A�@��;@��@�l�@��@��R@���@�M�@��@�{@��@��^@���@�/@���@��u@��D@�Z@�  @��m@��w@���@�|�@�+@��H@�ȴ@���@�~�@�^5@�=q@��@��T@���@�@���@��h@�`B@��@���@��u@�Z@��@�;@l�@
=@~ȴ@~v�@~5?@}�T@}@}O�@|z�@|�@{��@{C�@{@z�!@zn�@z�@y��@xĜ@x1'@w�w@w�w@w�@w|�@w+@v��@v�@v�R@v$�@up�@u/@u�@t��@t�@t�j@tZ@tI�@t9X@t1@s�
@sƨ@s��@st�@s33@r�@r��@r�@q��@q��@qX@p��@p�9@p1'@o��@o;d@o+@o�@nv�@n5?@n5?@m�-@mp�@l�j@lj@l9X@k�
@k��@kS�@k@j��@j~�@jJ@i7L@h��@h��@hQ�@h �@h  @g�@g��@g+@f�R@fv�@f$�@e@e�@eO�@d��@dZ@d1@c�@b�@b-@a�^@a%@`Ĝ@`�u@` �@_�P@_;d@^�R@^5?@]��@]V@\9X@[ƨ@[dZ@[C�@["�@Z�@Z�!@Zn�@Y��@Y��@Y%@XĜ@X��@XbN@XA�@W�;@W��@Wl�@W+@V�@V5?@U��@U/@T��@T�@So@R�\@R^5@R-@Q�#@Q��@Q��@Q�7@Qx�@QG�@Q%@P��@PQ�@P1'@O|�@O+@N��@N�R@N5?@N@M�h@L�D@L9X@L�@Kƨ@KS�@J�H@J�!@JM�@JJ@I�#@I��@H��@HbN@HA�@HA�@H1'@H1'@Gl�@G
=@F�R@E��@E/@EV@D�/@D�j@D(�@C�
@C��@C��@C��@C�@CdZ@Co@B��@B~�@B�@A�@A��@Ahs@A�@@bN@?�@?;d@>ȴ@>v�@>E�@=�@=�-@=p�@<��@<�D@<9X@;��@;�
@;�F@;��@;��@;��@;�@;dZ@;33@;"�@;"�@:�@:��@:�!@:=q@9�^@9��@9hs@9G�@9�@8��@8Ĝ@8�u@8bN@7�@7l�@6�y@6��@6E�@6@5��@5`B@5V@4�@4Z@3ƨ@3�F@333@2��@2��@2��@2n�@2J@1��@1x�@1X@1&�@0��@0�@0bN@0A�@01'@0 �@0b@/��@/�@.�R@.v�@.V@.E�@.$�@-�T@-�h@-`B@-O�@-?}@-?}@-/@-�@,��@,�/@,�j@,j@+��@+S�@+C�@+33@+o@*�H@*��@*�\@*-@)��@)��@)�^@)X@)7L@)&�@(��@(Ĝ@(��@(��@(��@(��@(bN@(1'@'�;@'�P@'|�@'\)@';d@'+@'�@'�@'
=@'
=@&�y@&��@&v�@&ff@&E�@&$�@&{@&@%�T@%@%�@$�@$��@$�@$�@$z�@$I�@$9X@$(�@$1@#�m@#ƨ@#�F@#t�@"�@"��@"��@"~�@"n�@"^5@"M�@"=q@"�@"J@!�#@!X@!�@ ��@ �@ r�@ r�@ Q�@  �@   @�w@�@��@�P@|�@\)@K�@�@�y@��@5?@{@�@��@�-@�-@�h@�@p�@?}@��@�/@�@�D@j@(�@ƨ@t�@33@�H@n�@M�@=q@�@J@��@�@��@��@�^@��@�7@7L@�`@�9@��@Q�@1'@��@|�@\)@��@�R@E�@��@p�@?}@�/@z�@j@I�@(�@1@�
@��@C�@o@�H@~�@-@��@hs@X@X@G�@G�@7L@�@�@%@%@%@%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ƨA���A���A���A�ĜA�Aܺ^Aܝ�A܁A�z�A�n�A�Q�A�?}A�7LA�1'A�$�A۴9A�VAٮA�A��HA��A�XA�Q�AɑhA�t�Aǰ!AƋDAŸRA���AÉ7A�p�A��wA��A��-A�K�A��A���A���A�ĜA��;A�`BA�E�A�G�A�JA�r�A���A���A��RA��;A�(�A���A��A��A�E�A��yA�x�A�7LA�JA���A��TA��yA�v�A��A�A�A��yA���A��A�ĜA�bA��yA�p�A�`BA���A�bA���A�7LA��PA�A��A��9A�1A��hA�%A�v�A�5?A�A�A��A��A�{A�ȴA�|�A��A��jA���A�  A�;dA��A��A�O�A�|�A��^A�VA��A�dZA~ȴA}�A{��Aw�;As��Aq�Ap5?Ap  AmƨAj�/Ah�Ag�mAgXAdjA`�A^�A^bA\�A[�AZA�AX�DAV(�AS&�AQ%AO��AN��ANAL�+AK|�AI�AHffAGK�AF�AFA�AE��AEG�AD�jAC��AC/AB��AB�AAdZA@-A>�A=��A<A�A: �A8��A6r�A5O�A4��A2^5A1��A0�DA/`BA-ƨA-/A-%A+��A*ZA)C�A(�A(�A&��A%ƨA%�A$��A#p�A"�+A!��A!;dA �A �DA (�A�^AO�A�A�A��A�A�mA"�A�\Al�A��A�wA��AdZA�AffA��A�A��A33An�A�wAdZA;dA��A��A�^A7LA9XA�mAO�A+AA�AO�A
�DA	��A	33A�A�jAdZA`BA �`A ĜA �!A ��A �+@���@���@�
=@�M�@�=q@���@��P@��@��h@���@��/@�dZ@�$�@�Ĝ@�C�@�o@�&�@���@��@�G�@���@��m@�;d@��@�\)@�l�@��y@��@�/@އ+@�-@�bN@�v�@٩�@�7L@��m@ְ!@���@ԃ@�C�@�@�hs@�V@��@ѩ�@�1'@�1'@Ͼw@Η�@�5?@͑h@���@�C�@�$�@ȓu@ư!@��T@�7L@���@���@���@�~�@��@�Q�@���@�v�@�hs@���@�A�@���@�K�@���@��!@���@�ff@��T@�A�@��
@��m@�  @�b@� �@���@��
@��w@��P@�t�@�^5@�O�@��@���@�K�@���@���@��P@�ȴ@�5?@��-@�?}@��@�V@��D@�ƨ@��@���@�S�@��+@��#@��7@�p�@�?}@���@�Ĝ@��9@��9@���@�I�@�  @��F@�t�@�t�@�t�@�\)@�K�@�;d@�+@���@��@���@��\@�@��-@��h@�x�@�hs@�hs@�O�@��@���@�I�@���@���@�t�@�;d@��@���@�M�@�=q@�5?@�$�@��@�J@��@���@�`B@�&�@��/@��u@�Q�@�b@���@���@�dZ@�S�@�C�@�33@�"�@�o@�
=@��@��H@���@�^5@���@���@�/@���@��9@�bN@� �@�b@��;@�t�@�33@�
=@��R@�v�@�-@���@��T@�`B@�V@��j@��@�z�@��u@��u@�Z@�1'@��
@�\)@�K�@�o@��@��H@���@��@��^@���@�p�@�`B@�&�@���@��@���@��@��m@���@��@�dZ@��y@���@�~�@�ff@�$�@�J@���@��^@���@���@�hs@�7L@�%@���@�b@��F@��@�S�@�+@�ȴ@���@�V@��@�@��h@�hs@�?}@�/@��`@��@��D@�Q�@�1'@�b@��@���@�l�@�S�@�C�@��@���@��@�ȴ@��!@�v�@�V@�{@���@��#@���@��^@���@�p�@�?}@���@��9@���@��D@�j@�A�@��;@��@�l�@��@��R@���@�M�@��@�{@��@��^@���@�/@���@��u@��D@�Z@�  @��m@��w@���@�|�@�+@��H@�ȴ@���@�~�@�^5@�=q@��@��T@���@�@���@��h@�`B@��@���@��u@�Z@��@�;@l�@
=@~ȴ@~v�@~5?@}�T@}@}O�@|z�@|�@{��@{C�@{@z�!@zn�@z�@y��@xĜ@x1'@w�w@w�w@w�@w|�@w+@v��@v�@v�R@v$�@up�@u/@u�@t��@t�@t�j@tZ@tI�@t9X@t1@s�
@sƨ@s��@st�@s33@r�@r��@r�@q��@q��@qX@p��@p�9@p1'@o��@o;d@o+@o�@nv�@n5?@n5?@m�-@mp�@l�j@lj@l9X@k�
@k��@kS�@k@j��@j~�@jJ@i7L@h��@h��@hQ�@h �@h  @g�@g��@g+@f�R@fv�@f$�@e@e�@eO�@d��@dZ@d1@c�@b�@b-@a�^@a%@`Ĝ@`�u@` �@_�P@_;d@^�R@^5?@]��@]V@\9X@[ƨ@[dZ@[C�@["�@Z�@Z�!@Zn�@Y��@Y��@Y%@XĜ@X��@XbN@XA�@W�;@W��@Wl�@W+@V�@V5?@U��@U/@T��@T�@So@R�\@R^5@R-@Q�#@Q��@Q��@Q�7@Qx�@QG�@Q%@P��@PQ�@P1'@O|�@O+@N��@N�R@N5?@N@M�h@L�D@L9X@L�@Kƨ@KS�@J�H@J�!@JM�@JJ@I�#@I��@H��@HbN@HA�@HA�@H1'@H1'@Gl�@G
=@F�R@E��@E/@EV@D�/@D�j@D(�@C�
@C��@C��@C��@C�@CdZ@Co@B��@B~�@B�@A�@A��@Ahs@A�@@bN@?�@?;d@>ȴ@>v�@>E�@=�@=�-@=p�@<��@<�D@<9X@;��@;�
@;�F@;��@;��@;��@;�@;dZ@;33@;"�@;"�@:�@:��@:�!@:=q@9�^@9��@9hs@9G�@9�@8��@8Ĝ@8�u@8bN@7�@7l�@6�y@6��@6E�@6@5��@5`B@5V@4�@4Z@3ƨ@3�F@333@2��@2��@2��@2n�@2J@1��@1x�@1X@1&�@0��@0�@0bN@0A�@01'@0 �@0b@/��@/�@.�R@.v�@.V@.E�@.$�@-�T@-�h@-`B@-O�@-?}@-?}@-/@-�@,��@,�/@,�j@,j@+��@+S�@+C�@+33@+o@*�H@*��@*�\@*-@)��@)��@)�^@)X@)7L@)&�@(��@(Ĝ@(��@(��@(��@(��@(bN@(1'@'�;@'�P@'|�@'\)@';d@'+@'�@'�@'
=@'
=@&�y@&��@&v�@&ff@&E�@&$�@&{@&@%�T@%@%�@$�@$��@$�@$�@$z�@$I�@$9X@$(�@$1@#�m@#ƨ@#�F@#t�@"�@"��@"��@"~�@"n�@"^5@"M�@"=q@"�@"J@!�#@!X@!�@ ��@ �@ r�@ r�@ Q�@  �@   @�w@�@��@�P@|�@\)@K�@�@�y@��@5?@{@�@��@�-@�-@�h@�@p�@?}@��@�/@�@�D@j@(�@ƨ@t�@33@�H@n�@M�@=q@�@J@��@�@��@��@�^@��@�7@7L@�`@�9@��@Q�@1'@��@|�@\)@��@�R@E�@��@p�@?}@�/@z�@j@I�@(�@1@�
@��@C�@o@�H@~�@-@��@hs@X@X@G�@G�@7L@�@�@%@%@%@%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B"�B"�B#�B%�B'�B)�B33B=qBK�BQ�BR�BT�B\)B^5B^5B^5B^5B_;Bw�Bv�BiyB]/BM�BB�BA�BdZBp�Bs�B|�Bz�Bu�Bu�Bn�B}�B��B�hB�+B|�By�B�B�bB�{B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�=B�Bz�Br�Bv�Bv�Bo�Bl�Bl�BdZBM�B2-BoBPBPBJBB��B�B�/B��B�}B��B��B�B��B�+Bp�BXBQ�BA�B/B'�BuBhB
��B
�sB
��B
�B
��B
��B
�hB
�1B
z�B
p�B
ffB
ZB
I�B
>wB
2-B
�B
B	�B	�B	�B	�;B	��B	��B	��B	��B	�}B	�B	��B	��B	�hB	�1B	~�B	q�B	`BB	P�B	F�B	<jB	7LB	/B	&�B	�B	�B	hB	JB	
=B	%B	B	B	  B��B��B��B��B��B�B�B�sB�NB��B��BŢB�jB�XB�B�B�B��B��B��B��B��B��B��B��B�{B�\B�hB�bB�oB�JB�JB�VB�DB�=B�7B�+B�B�B�B�B� B� B�B�B�B� B�B{�B{�Bv�Bx�Bv�Bs�Bo�Bk�Bm�Bn�Bn�Br�Bv�B{�B�DB��B��B��B��B��B��B��B��B��B��B��B�VB� B� B}�B�B�B�B�B�+B�B~�B}�B}�B}�B�B� By�B|�B�B�+B�B�B�B~�B~�Bx�Bk�BhsBjBiyBhsBiyBl�Bp�B{�B� B� B� B{�B{�B{�B|�B� B�B�B�=B�JB�1B�B�B�1B�=B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�!B�FB�^B�}BÖBĜBǮB��B��B��B�)B�5B�BB�ZB�B�B�B��B��B��B	  B	B	B	%B	B	B	B	B	%B	
=B	\B	�B	�B	 �B	!�B	#�B	$�B	&�B	&�B	+B	33B	6FB	6FB	5?B	9XB	=qB	A�B	B�B	C�B	F�B	I�B	J�B	K�B	L�B	N�B	Q�B	T�B	[#B	_;B	bNB	cTB	dZB	e`B	e`B	gmB	gmB	hsB	iyB	m�B	s�B	u�B	w�B	x�B	x�B	x�B	y�B	|�B	}�B	�B	�B	�B	�7B	�7B	�DB	�VB	�\B	�bB	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�XB	�jB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ÖB	ŢB	ƨB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�/B	�)B	�)B	�BB	�HB	�BB	�NB	�ZB	�TB	�TB	�`B	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
1B
+B
+B
1B

=B
	7B
	7B
DB
DB
DB
PB
JB
JB
JB
DB
DB
PB
\B
VB
PB
bB
\B
bB
bB
\B
bB
oB
oB
hB
oB
oB
oB
oB
{B
{B
{B
uB
uB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
)�B
'�B
(�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
(�B
+B
,B
,B
-B
-B
-B
-B
+B
,B
-B
-B
,B
-B
-B
,B
-B
-B
-B
-B
-B
.B
/B
1'B
2-B
1'B
1'B
2-B
2-B
2-B
33B
2-B
33B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
6FB
7LB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
=qB
>wB
=qB
=qB
>wB
>wB
<jB
?}B
?}B
?}B
>wB
>wB
?}B
?}B
@�B
@�B
@�B
?}B
@�B
B�B
B�B
B�B
A�B
?}B
A�B
A�B
@�B
A�B
E�B
D�B
D�B
D�B
F�B
G�B
H�B
H�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
F�B
G�B
G�B
H�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
M�B
L�B
N�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
R�B
Q�B
Q�B
VB
T�B
S�B
S�B
S�B
VB
VB
VB
T�B
VB
W
B
W
B
XB
XB
W
B
W
B
T�B
W
B
XB
YB
YB
YB
YB
YB
YB
[#B
[#B
[#B
[#B
ZB
ZB
ZB
ZB
YB
YB
YB
\)B
\)B
\)B
\)B
\)B
\)B
[#B
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
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
_;B
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
cTB
bNB
aHB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
ffB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
t�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B"�B"�B#�B%�B'�B*B3MB=�BK�BQ�BSBU2B\CB^OB^jB^�B_�BbNBx�By>Bn�BcTBT{BJ�BH1Bf�Br|Bv+B~�B}<Bx�Bx�Bs�B� B�YB�&B��B��B~(B�B�B�gB��B��B��B�NB��B�B��B��B��B�B��B��B��B��B��B��B��B��B}<Bt�Bw�Bw�BqBm]Bm)Be�BQ�B5�B�B�BvB�BYB�RB��B��B�pB��B��B�vB�aB��B��BsMBZ7BS&BC�B1�B+B�BBB
�B
�YB
��B
��B
��B
�[B
�#B
|�B
raB
h$B
\�B
L0B
@�B
4�B
#:B
tB	�|B	�B	�qB	�4B	�FB	�.B	�B	�jB	�GB	�-B	��B	��B	�B	�XB	��B	tB	c�B	TaB	I7B	>B	8lB	0�B	(�B	!bB	�B	@B	�B	
�B	B	�B	�B	 �B��B��B��B��B��B�OB�iB�B�ZBרB�B�1B��B��B��B�IB��B��B��B��B��B�xB�+B��B�KB��B� B��B�hB�[B��B��B�BB�B��B��B��B��B��B��B��B�UB�UB�3B�-B�B��B�B}qB}VBx�By�Bw�Bt�BqBl�Bn}Bo�Bo�BsBv�B{�B��B��B�B��B�OB��B�yB�KB�8B�
B�B�B�hB��B�B�4B��B�GB�GB�3B�B��B�OB~�B~wB~]B��B�oB{0B|�B�SB��B�B�B�B� B�Bz�Bn/Bj0BkBi�BiBi�Bl�Bp�B|B��B��B��B}qB|�B}<B~(B��B��B�B�B�B�7B�B��B��B�rB�NB�B��B��B�NB��B�XB��B��B�$B��B�$B�B��B��B��B��B��B��B��B�AB��B�0B�4B��B�B�1B�)B�BB�2B�CBބB��B�`B��B�B�B��B��B�(B	 4B	GB	SB	tB	�B	�B	�B	�B	tB	
�B	vB	eB	;B	!-B	"4B	$&B	%B	'8B	'RB	+�B	3MB	6`B	6�B	5�B	9�B	=�B	A�B	B�B	C�B	F�B	I�B	J�B	K�B	MB	O(B	R:B	U2B	[#B	_VB	b�B	cnB	dtB	e�B	e�B	g�B	g�B	h�B	i�B	m�B	s�B	u�B	w�B	x�B	x�B	y	B	zDB	}<B	~]B	�AB	�GB	�mB	�RB	��B	�xB	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�=B	�=B	�)B	�)B	�/B	�IB	�cB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B	�0B	�B	�B	�B	��B	�B	�,B	�2B	�MB	�gB	�EB	�EB	�7B	�7B	�KB	�yB	�QB	�CB	�]B	�OB	�dB	�xB	�xB	�\B	�|B	��B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�	B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B
 B
 B	�HB
 4B
 OB
'B
'B
-B
GB
-B
GB
aB
GB
mB
EB
KB
fB
_B
zB
fB

XB
	�B
	�B
^B
�B
xB
jB
~B
~B
~B
�B
�B
�B
vB
�B
�B
}B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 �B
"�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
$B
%B
$�B
%�B
%�B
&2B
%�B
&B
&B
&B
(
B
(
B
($B
(
B
*B
($B
)B
(>B
)*B
*B
*B
*B
*B
*B
)*B
*B
*KB
)DB
+B
,"B
,"B
-)B
-)B
-CB
-)B
+QB
,=B
-)B
-)B
,=B
-)B
-)B
,=B
-)B
-CB
-]B
-]B
-wB
.cB
/iB
1AB
2GB
1[B
1vB
2aB
2|B
2aB
3�B
2|B
3�B
5�B
6zB
7�B
7fB
7fB
7fB
7fB
6zB
7�B
7�B
9�B
9�B
9�B
9�B
9rB
9rB
9rB
9rB
8�B
8�B
8�B
8�B
8�B
8�B
7�B
:�B
;B
;B
;B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
;�B
=�B
>�B
=�B
=�B
>�B
>�B
<�B
?�B
?�B
?�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
?�B
@�B
B�B
B�B
B�B
A�B
?�B
A�B
A�B
@�B
A�B
E�B
D�B
D�B
D�B
F�B
G�B
H�B
H�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
F�B
G�B
G�B
H�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
OB
OB
N�B
NB
MB
N�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
NB
NB
OB
O�B
O�B
Q B
Q B
Q B
Q B
RB
QB
Q4B
S&B
R B
R B
VB
UB
T,B
TFB
TB
V9B
VB
VB
UB
V9B
W$B
W$B
XB
X+B
W$B
W$B
U2B
W$B
X+B
Y1B
YKB
Y1B
Y1B
Y1B
YKB
[#B
[=B
[#B
[#B
Z7B
Z7B
ZQB
Z7B
YKB
YKB
YKB
\CB
\CB
\]B
\CB
\CB
\CB
[WB
\]B
]IB
]IB
]IB
]IB
^5B
^jB
^OB
^OB
_;B
_VB
_;B
^jB
^OB
^OB
^OB
_;B
`\B
`\B
`BB
`\B
`BB
`BB
`\B
`\B
_VB
`\B
aHB
abB
abB
aHB
aHB
a|B
`\B
`\B
_�B
a|B
bhB
bNB
bhB
bhB
bNB
cTB
cnB
bhB
b�B
cnB
b�B
a�B
cnB
d�B
dtB
dZB
dZB
dtB
dZB
dtB
dtB
d�B
c�B
dtB
ezB
ezB
ffB
ffB
f�B
f�B
f�B
f�B
gmB
g�B
gmB
gmB
g�B
g�B
g�B
f�B
g�B
f�B
hsB
h�B
h�B
iyB
iyB
i�B
iyB
i�B
i�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
t�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708090033412017080900334120170809003341201806221317142018062213171420180622131714201804050719252018040507192520180405071925  JA  ARFMdecpA19c                                                                20170805093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170805003601  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170805003603  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170805003603  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170805003604  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170805003604  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170805003604  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170805003604  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170805003604  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170805003605                      G�O�G�O�G�O�                JA  ARUP                                                                        20170805005634                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170805154110  CV  JULD            G�O�G�O�F�߮                JM  ARCAJMQC2.0                                                                 20170808153341  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170808153341  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221925  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                