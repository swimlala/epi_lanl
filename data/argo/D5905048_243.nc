CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-23T00:35:29Z creation;2018-05-23T00:35:33Z conversion to V3.1;2019-12-19T07:37:41Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180523003529  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_243                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�d��kT�1   @�d���O�@4[6z���dM1���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A��AffA@  A`  A���A�  A�33A�33A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�C3D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
=@}p�@��A ��AA?\)A_\)A�z�A��A��HA��HA��AϮA߮A�z�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;�)C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C\\C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA��DA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDaw
Da�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�{�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D��D�A�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�{�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D���D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��
A��#A��A���A���A���A���A���A���A���A���A̶FA̟�A�-A˓uA�+A��Aʛ�A�`BA�+A���A�z�A�`BA�E�A��#A��yAȺ^A���A�C�A�bAƺ^AƋDA� �A�z�A�Q�A¬A+A��A�r�A�S�A���A�|�A�x�A�v�A�S�A��/A�ffA��#A���A��A��hA��HA�x�A��A��mA���A��-A�~�A�&�A�33A�`BA���A�A�A�7LA���A��A��/A�-A��7A�C�A��mA�(�A��!A���A��7A�(�A��-A�&�A��/A��
A�oA�jA���A��A�G�A��A�S�A�v�A���A�?}A��yA���A�Q�A��TA��DA��
A���A���A�x�A�  A� �A�ZA�+A�?}A��
A���A���A���A��-A�5?A�hsA�ȴA�ȴA���A�I�A�E�A�K�A�#A|��Av��ApȴAm�wAk33AgdZAfĜAf�Ac�Aa��A`�DA^��AY�^AXE�AW�AU�
AT��AR��AQ�wAP��AMp�AJ�RAJZAH��AG�AG�AFM�AD�!AB�ABbAA%A@��A?�A=�wA<1A;�wA9��A9�7A9+A8�\A8ZA7S�A5S�A4M�A2��A0��A/|�A.A-/A,�A+A)�A'�;A&��A&bA$ȴA#�TA"�uA!��A!S�A�A��A�+An�AA�PA�A|�A��AK�AE�AdZA7LA�uA��A33A5?A�DA��A{A�A��A�A�\At�A-A
��A
bA	C�A�AffAhsA��A~�A=qA��Av�A��AA�PAoA z�A  �A @�o@���@�n�@�$�@���@�"�@��@��`@��@��9@��@�1@�@�R@��#@�I�@ꟾ@�5?@�j@��
@�F@�-@�V@�5?@�hs@�Q�@ۥ�@ڟ�@ٺ^@��@�x�@���@ԓu@�1'@ҏ\@�p�@�Ĝ@�l�@��/@�z�@� �@��
@ˍP@�~�@�hs@ȃ@�\)@Ƨ�@Ł@��@���@ċD@å�@���@�J@���@�/@�1'@�dZ@�C�@�"�@���@���@���@��^@��h@�hs@��@�Z@���@��^@��@�?}@�S�@���@���@���@���@�p�@�X@��u@���@��P@��P@�dZ@�o@��H@�J@�7L@��/@�I�@��@�l�@�o@�v�@�E�@�J@��-@���@�Ĝ@��D@�(�@��m@��@��@��@��\@�n�@�=q@��@���@��@��@���@�K�@��H@��@��H@�
=@���@�V@�@���@�?}@���@��@�I�@�b@��@���@�dZ@�+@�ȴ@���@���@�^5@�J@��-@��@�hs@��/@� �@� �@��D@���@�b@���@���@��P@�33@���@�E�@��@���@�x�@�O�@�7L@���@��/@��/@��j@�I�@�b@�b@�  @��@��w@��@�+@��!@�=q@��#@�/@���@�Ĝ@�z�@�9X@� �@��@��@�o@�ȴ@��!@���@��+@�^5@�^5@�V@�-@��@��#@�@���@�x�@�?}@��`@�Ĝ@���@��@�9X@���@���@�dZ@��H@�~�@�ff@�V@�E�@�5?@�{@���@���@���@���@���@�x�@��@���@���@��j@���@��@�A�@�b@��F@���@��@�C�@�
=@���@��@��R@�~�@�^5@�=q@��T@��7@�?}@�V@���@��@�r�@��@�ƨ@�|�@�"�@���@��y@���@��+@�@���@��^@��7@�7L@�%@���@�I�@�9X@�  @��m@���@��F@���@�S�@���@�ff@�5?@��#@���@�hs@�O�@�/@��@���@��@���@��j@��9@���@��@��@�(�@�  @��m@��
@�|�@�ȴ@�~�@�V@�@�@��-@��^@��-@��-@���@��7@�G�@��@�z�@�Q�@�A�@�1'@~�y@~v�@~5?@~$�@}?}@}/@}V@|9X@|1@{��@{�m@{�F@{t�@{33@{"�@zn�@z�!@z��@z��@z-@y��@y��@yX@y&�@x��@x1'@w�w@w�P@wl�@w
=@vv�@v@u@uO�@u�@uV@t��@sƨ@s��@sdZ@s33@r�!@r=q@rJ@qhs@p��@p  @o��@o\)@o�@nV@n{@m��@m`B@m�@l�@l9X@l�@l�@l1@kƨ@k��@kC�@k@j-@i�#@i��@i��@i�7@iG�@i%@h��@hQ�@h �@g�@g�;@g�;@g��@gK�@f�R@f�+@eO�@d�j@dz�@d�@c�F@co@b�!@b~�@bn�@bM�@a��@a��@ax�@aG�@a%@`�`@`Ĝ@`�9@`Q�@_�@_+@^�+@^V@]�@]�T@]�-@]�h@]p�@]`B@]O�@]/@]V@\�@\��@[��@Z~�@Z^5@ZM�@ZJ@Y��@Y�^@X�`@X��@X �@W�;@W�@W\)@V�@VE�@U�@U@U�h@U�@T��@T�j@S�
@S�@SC�@R�@R��@R�\@R^5@RM�@Q��@QG�@P�9@O�;@Ol�@Nȴ@N{@M�T@MV@L�@K�
@KS�@K@J=q@J-@J�@I��@Ihs@IG�@H��@HbN@HA�@H  @Gl�@F��@F�y@Fv�@E�@E@E�@E?}@E/@D��@D�@Dz�@DZ@D1@C�F@C�@Ct�@CdZ@C"�@B�@B^5@A��@A�#@AX@@��@@Ĝ@@r�@@ �@@b@?�@?�w@?�P@?\)@?
=@>��@>5?@=��@=�@=O�@=?}@=�@<��@<�j@<�D@<Z@;�m@;t�@;o@:��@:��@:�\@:n�@:-@:J@9��@9x�@97L@8��@8�@8bN@8Q�@8b@7�@7l�@6�@6v�@5@5�@5p�@5`B@5O�@5V@4��@4��@4z�@49X@4�@41@3��@3�m@3ƨ@3dZ@3o@2M�@1��@1��@1&�@0��@0�@0bN@0A�@0 �@0b@/�w@/��@/�;@/�w@/\)@.ȴ@.�+@.V@.@-�-@-p�@-O�@-V@,�D@,Z@,I�@,(�@,�@,�@,1@+�
@+ƨ@+��@+dZ@+o@*�H@*��@*�!@*^5@*J@)�7@)X@)7L@(��@(�9@(�@(bN@(Q�@(A�@'��@'K�@'
=@&�y@&��@&��@&�+@&ff@&$�@%�@%�-@%�@%�@%O�@%�@%V@$�j@$Z@$�@$1@#�m@#�F@#��@#�@#dZ@#o@"�\@"n�@"=q@!�@!��@!�7@!x�@!G�@!7L@ �`@ ��@ r�@ bN@ Q�@ 1'@  �@ b@�;@�@��@��@��@|�@;d@
=@�@�R@�+@ff@ff@ff@E�@$�@{@�@@�h@�h@p�@�@�@��@��@�D@�D@�D@Z@9X@(�@��@��@dZ@o@�H@�!@n�@-@�@�^@�^@��@x�@G�@%@�`@��@��@r�@bN@A�@b@  @�;@��@\)@+@�+@ff@V@{@�-@�h@p�@?}@V@�@�j@z�@9X@(�@�@�
@��@t�@dZ@S�@C�@C�@�@��@^5@^5@^5@^5@=q@��@�^@��@�7@hs@G�@&�@Ĝ@r�@1'@  @  @�;@�w@\)@+@
=@�R@�+@v�@ff@5?@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��
A��#A��A���A���A���A���A���A���A���A���A̶FA̟�A�-A˓uA�+A��Aʛ�A�`BA�+A���A�z�A�`BA�E�A��#A��yAȺ^A���A�C�A�bAƺ^AƋDA� �A�z�A�Q�A¬A+A��A�r�A�S�A���A�|�A�x�A�v�A�S�A��/A�ffA��#A���A��A��hA��HA�x�A��A��mA���A��-A�~�A�&�A�33A�`BA���A�A�A�7LA���A��A��/A�-A��7A�C�A��mA�(�A��!A���A��7A�(�A��-A�&�A��/A��
A�oA�jA���A��A�G�A��A�S�A�v�A���A�?}A��yA���A�Q�A��TA��DA��
A���A���A�x�A�  A� �A�ZA�+A�?}A��
A���A���A���A��-A�5?A�hsA�ȴA�ȴA���A�I�A�E�A�K�A�#A|��Av��ApȴAm�wAk33AgdZAfĜAf�Ac�Aa��A`�DA^��AY�^AXE�AW�AU�
AT��AR��AQ�wAP��AMp�AJ�RAJZAH��AG�AG�AFM�AD�!AB�ABbAA%A@��A?�A=�wA<1A;�wA9��A9�7A9+A8�\A8ZA7S�A5S�A4M�A2��A0��A/|�A.A-/A,�A+A)�A'�;A&��A&bA$ȴA#�TA"�uA!��A!S�A�A��A�+An�AA�PA�A|�A��AK�AE�AdZA7LA�uA��A33A5?A�DA��A{A�A��A�A�\At�A-A
��A
bA	C�A�AffAhsA��A~�A=qA��Av�A��AA�PAoA z�A  �A @�o@���@�n�@�$�@���@�"�@��@��`@��@��9@��@�1@�@�R@��#@�I�@ꟾ@�5?@�j@��
@�F@�-@�V@�5?@�hs@�Q�@ۥ�@ڟ�@ٺ^@��@�x�@���@ԓu@�1'@ҏ\@�p�@�Ĝ@�l�@��/@�z�@� �@��
@ˍP@�~�@�hs@ȃ@�\)@Ƨ�@Ł@��@���@ċD@å�@���@�J@���@�/@�1'@�dZ@�C�@�"�@���@���@���@��^@��h@�hs@��@�Z@���@��^@��@�?}@�S�@���@���@���@���@�p�@�X@��u@���@��P@��P@�dZ@�o@��H@�J@�7L@��/@�I�@��@�l�@�o@�v�@�E�@�J@��-@���@�Ĝ@��D@�(�@��m@��@��@��@��\@�n�@�=q@��@���@��@��@���@�K�@��H@��@��H@�
=@���@�V@�@���@�?}@���@��@�I�@�b@��@���@�dZ@�+@�ȴ@���@���@�^5@�J@��-@��@�hs@��/@� �@� �@��D@���@�b@���@���@��P@�33@���@�E�@��@���@�x�@�O�@�7L@���@��/@��/@��j@�I�@�b@�b@�  @��@��w@��@�+@��!@�=q@��#@�/@���@�Ĝ@�z�@�9X@� �@��@��@�o@�ȴ@��!@���@��+@�^5@�^5@�V@�-@��@��#@�@���@�x�@�?}@��`@�Ĝ@���@��@�9X@���@���@�dZ@��H@�~�@�ff@�V@�E�@�5?@�{@���@���@���@���@���@�x�@��@���@���@��j@���@��@�A�@�b@��F@���@��@�C�@�
=@���@��@��R@�~�@�^5@�=q@��T@��7@�?}@�V@���@��@�r�@��@�ƨ@�|�@�"�@���@��y@���@��+@�@���@��^@��7@�7L@�%@���@�I�@�9X@�  @��m@���@��F@���@�S�@���@�ff@�5?@��#@���@�hs@�O�@�/@��@���@��@���@��j@��9@���@��@��@�(�@�  @��m@��
@�|�@�ȴ@�~�@�V@�@�@��-@��^@��-@��-@���@��7@�G�@��@�z�@�Q�@�A�@�1'@~�y@~v�@~5?@~$�@}?}@}/@}V@|9X@|1@{��@{�m@{�F@{t�@{33@{"�@zn�@z�!@z��@z��@z-@y��@y��@yX@y&�@x��@x1'@w�w@w�P@wl�@w
=@vv�@v@u@uO�@u�@uV@t��@sƨ@s��@sdZ@s33@r�!@r=q@rJ@qhs@p��@p  @o��@o\)@o�@nV@n{@m��@m`B@m�@l�@l9X@l�@l�@l1@kƨ@k��@kC�@k@j-@i�#@i��@i��@i�7@iG�@i%@h��@hQ�@h �@g�@g�;@g�;@g��@gK�@f�R@f�+@eO�@d�j@dz�@d�@c�F@co@b�!@b~�@bn�@bM�@a��@a��@ax�@aG�@a%@`�`@`Ĝ@`�9@`Q�@_�@_+@^�+@^V@]�@]�T@]�-@]�h@]p�@]`B@]O�@]/@]V@\�@\��@[��@Z~�@Z^5@ZM�@ZJ@Y��@Y�^@X�`@X��@X �@W�;@W�@W\)@V�@VE�@U�@U@U�h@U�@T��@T�j@S�
@S�@SC�@R�@R��@R�\@R^5@RM�@Q��@QG�@P�9@O�;@Ol�@Nȴ@N{@M�T@MV@L�@K�
@KS�@K@J=q@J-@J�@I��@Ihs@IG�@H��@HbN@HA�@H  @Gl�@F��@F�y@Fv�@E�@E@E�@E?}@E/@D��@D�@Dz�@DZ@D1@C�F@C�@Ct�@CdZ@C"�@B�@B^5@A��@A�#@AX@@��@@Ĝ@@r�@@ �@@b@?�@?�w@?�P@?\)@?
=@>��@>5?@=��@=�@=O�@=?}@=�@<��@<�j@<�D@<Z@;�m@;t�@;o@:��@:��@:�\@:n�@:-@:J@9��@9x�@97L@8��@8�@8bN@8Q�@8b@7�@7l�@6�@6v�@5@5�@5p�@5`B@5O�@5V@4��@4��@4z�@49X@4�@41@3��@3�m@3ƨ@3dZ@3o@2M�@1��@1��@1&�@0��@0�@0bN@0A�@0 �@0b@/�w@/��@/�;@/�w@/\)@.ȴ@.�+@.V@.@-�-@-p�@-O�@-V@,�D@,Z@,I�@,(�@,�@,�@,1@+�
@+ƨ@+��@+dZ@+o@*�H@*��@*�!@*^5@*J@)�7@)X@)7L@(��@(�9@(�@(bN@(Q�@(A�@'��@'K�@'
=@&�y@&��@&��@&�+@&ff@&$�@%�@%�-@%�@%�@%O�@%�@%V@$�j@$Z@$�@$1@#�m@#�F@#��@#�@#dZ@#o@"�\@"n�@"=q@!�@!��@!�7@!x�@!G�@!7L@ �`@ ��@ r�@ bN@ Q�@ 1'@  �@ b@�;@�@��@��@��@|�@;d@
=@�@�R@�+@ff@ff@ff@E�@$�@{@�@@�h@�h@p�@�@�@��@��@�D@�D@�D@Z@9X@(�@��@��@dZ@o@�H@�!@n�@-@�@�^@�^@��@x�@G�@%@�`@��@��@r�@bN@A�@b@  @�;@��@\)@+@�+@ff@V@{@�-@�h@p�@?}@V@�@�j@z�@9X@(�@�@�
@��@t�@dZ@S�@C�@C�@�@��@^5@^5@^5@^5@=q@��@�^@��@�7@hs@G�@&�@Ĝ@r�@1'@  @  @�;@�w@\)@+@
=@�R@�+@v�@ff@5?@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
hB
0!B
A�B
G�B
L�B
L�B
M�B
^5B
�B
��B
�9BJBP�BXB_;Bn�Bu�B�B�B�VB�1BĜB�B��B��B��B%B%BDBJBJB%B1BJB�B�B1'BG�BI�BA�B5?B?}B49BhB�B�B'�B33BD�BYBZBffBR�B/B.B�B�B�BVB��B��BB��B�B�B�B�ZB��B�#BŢB��B��BɺB�qB�B��B�=BgmBe`Bk�B{�B}�Bw�Bm�BffBaHBZBH�B=qB/B �B$�B�B1B
�B
��B
��B
��B
�=B
aHB
T�B
6FB
�B	�`B	�BB	�/B	�FB	�+B	�uB	z�B	k�B	u�B	s�B	S�B	I�B	F�B	(�B	  B	�B	$�B	�B	�B	B��B��B�/B�B��B�mB�B�sB�;B��B��B�B��B�B��B�dB�'BĜB�3B�qB�wB�FB�?B��B�{B��B�DB�B�%B�B�+B�B{�Bz�BhsBw�Bv�Bq�Bp�Bo�Bo�Bs�BiyBe`Bz�Bx�Bo�Bp�BgmB\)B^5B\)B[#B`BBhsB`BBYBZBN�BE�BC�BXB_;BYBR�BO�BE�B?}B=qBG�BB�B<jB:^BJ�BJ�B[#BYBQ�BO�BXBe`BaHB]/B^5BdZBhsBdZBffBiyBgmB`BB\)B`BB_;BXBT�BP�B_;Bm�BjBiyBgmBiyBp�Bl�BiyB_;BhsBn�BbNBq�Bp�Br�Bo�Bl�BdZBs�B~�B�B�B{�B~�B�B~�B|�B�JB�VB�PB�JB�+B�+B�=B�DB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�9B�9B�3B�'B�!B�!B�^BŢBǮBŢB��B��B�B�)B�)B�/B�/B�`B�B��B��B��B��B��B��B	B	B	%B	VB	VB	\B	�B	�B	{B	�B	�B	�B	�B	 �B	!�B	!�B	'�B	(�B	/B	1'B	33B	5?B	5?B	2-B	/B	;dB	?}B	D�B	H�B	M�B	N�B	O�B	T�B	W
B	XB	ZB	_;B	^5B	`BB	cTB	dZB	jB	l�B	m�B	p�B	s�B	t�B	s�B	u�B	v�B	w�B	v�B	w�B	�B	�B	�+B	�%B	�=B	�=B	�=B	�1B	�1B	�1B	�DB	�DB	�DB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�-B	�?B	�RB	�dB	�jB	�jB	�jB	�wB	�qB	�qB	�qB	��B	�}B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�)B	�)B	�5B	�5B	�5B	�5B	�BB	�BB	�HB	�HB	�HB	�TB	�`B	�fB	�`B	�fB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB

=B
DB
JB
DB
	7B
+B
DB
JB
JB
VB
bB
{B
{B
�B
{B
uB
oB
oB
hB
uB
uB
uB
bB
hB
hB
oB
bB
uB
uB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
#�B
#�B
#�B
"�B
"�B
#�B
"�B
$�B
#�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
)�B
)�B
(�B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
-B
-B
+B
+B
,B
(�B
+B
-B
-B
,B
,B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
/B
/B
/B
0!B
2-B
2-B
33B
33B
33B
33B
33B
49B
33B
33B
33B
2-B
0!B
/B
5?B
6FB
5?B
5?B
5?B
33B
6FB
5?B
6FB
7LB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
9XB
8RB
7LB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
;dB
;dB
>wB
=qB
>wB
=qB
?}B
?}B
>wB
@�B
@�B
@�B
?}B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
E�B
E�B
E�B
D�B
D�B
E�B
D�B
E�B
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
H�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
K�B
L�B
L�B
M�B
M�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
P�B
P�B
P�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
Q�B
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
W
B
VB
VB
VB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
\)B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
`BB
_;B
_;B
`BB
`BB
_;B
_;B
_;B
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
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
e`B
e`B
ffB
e`B
e`B
dZB
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
ffB
gmB
ffB
ffB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
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
jB
jB
jB
jB
k�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
k�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
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
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�B
�B
0�B
B'B
HKB
MPB
MjB
N�B
^�B
�GB
�B
�tB~BQ4BY�B`BBoBvzB��B�{B�HB��B�mB�SB��B�NB�?B+BtBDB~B�BEB	RBjB_B 'B2-BH�BJ�BB�B7�B@�B6�B2BB�B'�B2�BEBZ�B\)BhsBV�B3B/�B!B�B�BHB��B��B�B�6B�}B��B��B�BҽB��B��B̘B�6B��B�B��B�&B��Bk�Be�Bl�B|�B~�By�Bo�BhsBb�B[qBJ�B?}B1�B"�B%�B�B
�B
ܬB
�vB
͹B
�6B
�VB
e�B
W�B
9�B
# B	�=B	�&B	��B	��B	�B	��B	~]B	o5B	v�B	t�B	W$B	LB	H�B	,B	�B	 \B	&�B	QB	�B	aB	 �B��B�B�KB�XB�DB�B�B�B�$B��B�$B�FB��B�}B�B��B�B�%B�B��B�LB��B��B�
B�B��B�mB�1B��B�KB��B}�B|�Bk6By$BxBshBrBqABp�Bt�BkkBgBz�By>Bp�Bq'Bh�B^B_�B]�B\�BabBiBaHBZQB[=BP�BG�BE�BX�B_�BY�BS�BP�BG_BAUB?.BH�BC�B>]B<jBLBK�B[WBY�BSBQNBX�BezBa�B^B_Bd�Bh�Bd�Bf�Bi�Bg�BaHB]~Ba-B`'BY�BV�BR�B`BBm�BkkBjKBh�Bj�Bq'Bm�Bj�Ba�Bi�Bo�BdBr-BqvBshBpoBmwBfLBt�BcB�gB��B}"B�B��B�B~�B�~B��B��B��B�B��B��B�B�B�FB��B��B�B�QB�]B�BB�@B�tB��B�sB�OB�OB��B��B�hB�TB�nB�hB��B��B�AB�B��B�KB��B�FB՛B�KB�CB�]B�~B��B��B�B��B�B�B�8B��B��B	mB	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	B	 �B	"B	"4B	($B	)DB	/5B	1vB	3hB	5tB	5tB	2�B	0B	;�B	?�B	D�B	H�B	M�B	OBB	PHB	UMB	WYB	XyB	ZkB	_pB	^�B	`vB	cnB	d�B	j�B	l�B	m�B	p�B	s�B	t�B	tB	u�B	v�B	xB	w2B	xRB	��B	��B	�EB	��B	�rB	�rB	�rB	��B	��B	��B	�xB	�xB	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�HB	�:B	�2B	�XB	�QB	�5B	�IB	�UB	�GB	�aB	�|B	��B	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	�B	��B	��B	�0B	�(B	�&B	�B	�B	�B	�,B	�,B	�9B	�B	�+B	�+B	�?B	�?B	�CB	�]B	�jB	�jB	�jB	ބB	�vB	��B	�|B	�bB	�|B	�B	�zB	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�	B	�B	�B	�B	�<B	�(B	�(B	�B	�BB	�]B	�qB
 OB
AB
uB
GB
9B
?B
?B
EB
EB
EB
fB
fB
	7B
	RB

XB
^B

�B
^B
dB
xB
	�B
�B
xB
~B
�B
�B
}B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 �B
!�B
!�B
"�B
!�B
!B
#�B
#�B
#�B
# B
# B
$B
# B
%B
$@B
&B
&B
&2B
&2B
(
B
(
B
(>B
)*B
)DB
*B
+B
+B
+B
+B
+B
*0B
*B
)_B
+B
,B
,"B
,"B
,"B
,"B
,"B
,"B
-CB
-CB
.B
-)B
-CB
+6B
+6B
,=B
)_B
+6B
-)B
-CB
,=B
,WB
./B
/OB
/OB
/5B
/5B
/5B
/5B
/5B
0;B
0;B
0;B
0;B
/OB
/iB
/OB
0UB
2GB
2GB
3MB
3MB
3MB
3hB
3MB
4TB
3hB
3MB
3MB
2aB
0�B
/�B
5?B
6FB
5tB
5ZB
5ZB
3�B
6`B
5�B
6`B
7�B
6zB
6�B
6zB
7fB
8lB
8�B
8�B
9rB
8�B
7�B
9rB
:xB
:xB
:xB
:xB
:xB
:xB
9�B
9�B
:�B
:�B
;�B
;�B
;�B
<�B
;�B
;�B
>�B
=�B
>�B
=�B
?}B
?�B
>�B
@�B
@�B
@�B
?�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
E�B
E�B
E�B
D�B
D�B
E�B
D�B
E�B
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
H�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
K�B
MB
L�B
M�B
NB
L�B
NB
MB
NB
M�B
M�B
N"B
OB
O�B
N�B
OB
N�B
OB
PB
OB
O�B
P�B
P�B
Q B
O�B
O�B
PB
Q B
QB
Q B
Q�B
RB
RB
R B
QB
Q4B
P.B
R:B
SB
S&B
SB
UB
U2B
UB
UB
UB
UB
VB
W
B
VB
V9B
VSB
XEB
X+B
XEB
X+B
Y1B
Y1B
Y1B
YKB
ZQB
[#B
[=B
[#B
\)B
[=B
[=B
[=B
[=B
[WB
[=B
\CB
\CB
\CB
\]B
\CB
\xB
]IB
]IB
]IB
^OB
^jB
^jB
^OB
^jB
^jB
]dB
_pB
_VB
`\B
`\B
`\B
`\B
_pB
`\B
_pB
_VB
`BB
`\B
_VB
_VB
_pB
_VB
`\B
aHB
abB
a|B
a|B
abB
a|B
`vB
`�B
abB
abB
abB
b�B
bhB
cTB
cnB
cnB
c�B
cnB
d�B
dtB
dZB
dtB
e`B
e`B
dtB
ezB
e`B
ffB
ezB
ezB
dtB
ezB
f�B
f�B
f�B
gmB
gmB
g�B
g�B
f�B
gmB
f�B
f�B
f�B
g�B
g�B
f�B
g�B
g�B
g�B
hsB
iyB
iyB
i�B
i�B
i�B
i�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
k�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
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
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805270037462018052700374620180527003746201806221330562018062213305620180622133056201806042133252018060421332520180604213325  JA  ARFMdecpA19c                                                                20180523093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180523003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180523003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180523003532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180523003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180523003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180523003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180523003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180523003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180523003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20180523005729                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180523153543  CV  JULD            G�O�G�O�F�%�                JM  ARGQJMQC2.0                                                                 20180523153543  CV  JULD_LOCATION   G�O�G�O�F�%�                JM  ARGQJMQC2.0                                                                 20180523153543  CV  LATITUDE        G�O�G�O�A���                JM  ARCAJMQC2.0                                                                 20180526153746  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180526153746  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123325  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043056  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                