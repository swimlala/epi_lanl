CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-08T00:35:36Z creation;2018-04-08T00:35:40Z conversion to V3.1;2019-12-19T07:41:14Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180408003536  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_228                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�YvD�[�1   @�Yw`� @4�@��4n�dHU2a|1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDzw
Dz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D���D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��;A��;A��;A��
A���A�ȴA�ƨA�ƨA�ȴA�ȴA�ƨAʹ9A͡�A�+A���A�r�A�E�A��AȓuA�bNA�A�ƨAƺ^A�-A��TAŋDA�+AđhA�C�A×�A���A���Aº^A�{A�ƨA�M�A��A��+A��A��9A��A�"�A���A�dZA�9XA��A�VA���A��HA��A��\A�I�A�bA���A�/A�v�A��DA��A���A��;A�C�A��A��hA�(�A��A��PA�A��A��A���A�VA��A�l�A��jA���A���A���A��mA�JA�oA�VA�E�A��A�
=A���A�G�A�ZA��\A�E�A�Q�A��A���A��A��A�5?A��DA���A�33A�n�A�S�A���A�jA�=qA��A���A��7A�t�A��7A�C�A�hsA�|�A��7A�A�`BA�|�A~�Av��AshsAr��Ar��Ar1'Ao�
Am�Am�AmdZAl��Aj�9Ag�mAg
=Ae��Ad(�Ac�hAb�+A`(�A_O�A^I�A[��A[x�AZ�HAZVAW��AV  AUG�AT�RAS�AQ��AO��AN  AJ��AI�AG�mAE��AD�ADE�ACAC&�AB1A@��A<��A:�A9O�A7�A7dZA5ƨA2ĜA1��A1
=A/��A/�A.-A,�yA,E�A+��A)��A)��A)�A&�`A%hsA$�A"��A!hsA v�A M�A {AO�A��A�A"�A�\Al�AA�`A�;A�AQ�AdZA��AI�A�hAhsA�An�A��A�A��A
�/A	"�A��AbAdZAVA�DAXA�jA��A�A1AS�A �/A   @�S�@��@���@�v�@��T@��7@�;d@�E�@���@��P@��\@�@��u@�R@�{@��@�1@�K�@���@�ƨ@�t�@�@�+@�X@睲@䛦@�j@��m@�~�@�?}@�j@� �@�33@���@��H@��T@�|�@�v�@١�@���@ם�@�33@�+@���@��#@��`@��@�;d@��@ҏ\@ҧ�@�-@�&�@�Ĝ@Гu@� �@�C�@��@˝�@ʰ!@ʏ\@�=q@�O�@ȴ9@�z�@�dZ@��@��/@�1'@�"�@���@�(�@�n�@��u@�A�@�1@��
@��@�C�@��!@��@�G�@� �@��@��@�O�@�V@���@�j@��@���@��y@���@��\@�~�@�n�@�$�@��#@��^@�G�@��@���@���@�I�@�S�@�
=@���@���@�hs@�O�@���@�bN@�b@��
@�|�@�o@���@�~�@�`B@�V@��9@�b@��P@�o@���@�n�@�=q@�@�O�@��@��@��u@��@��@�t�@�l�@�o@��\@�M�@���@�7L@���@��/@��D@�I�@�I�@��;@��w@��
@���@��@��P@�\)@�S�@�"�@�o@��y@�ȴ@�~�@�=q@�{@�J@��@�@��h@�x�@�O�@�7L@�5?@�^5@�=q@�-@�-@�-@��h@�1'@�A�@�Ĝ@��9@���@��9@��u@�z�@�Q�@�b@���@�t�@�"�@�@���@�^5@�J@��#@��T@��T@��#@�@���@�p�@�`B@�7L@�/@��@��@�Q�@�  @��
@��@�|�@�S�@��@���@�^5@�=q@�@�`B@�7L@��@�%@��`@���@��j@��u@�z�@�bN@��
@�|�@�dZ@�"�@��H@���@�^5@��@���@��#@�@��@��@���@��D@�bN@��@�l�@�
=@��y@��\@�V@�J@��#@���@���@�`B@��`@�Z@�(�@���@�l�@�K�@�;d@�+@��@�
=@��H@�ȴ@���@��!@���@��+@���@�X@�?}@�7L@�/@�/@�&�@�&�@��@�V@�%@��/@��j@�z�@�A�@��
@�\)@���@��!@��\@�-@��@���@��7@�`B@�&�@���@��j@���@��D@�Z@�1'@�  @���@���@��
@���@�t�@�\)@�K�@�o@��@���@�ff@�@���@��h@�`B@�/@��@��@��`@��/@���@���@���@���@��@K�@~��@~�+@~V@~{@|�j@|(�@{��@{t�@{o@z~�@zM�@z=q@y�@xĜ@xr�@xb@w�w@w\)@v�@vȴ@v��@v��@v�+@vv�@v$�@u/@t�@t�@t(�@s��@s�F@st�@r�!@rJ@qx�@q�@q�@q%@p�@o��@o�@nȴ@nv�@n{@m�-@mO�@mV@l�@lI�@k��@k@jM�@i�@i�^@i��@i�7@i&�@i&�@i%@hĜ@h��@hbN@h1'@h  @g��@g��@g
=@f�R@fv�@e�T@e�@eO�@eV@d��@d��@d��@dj@dI�@d9X@d9X@d1@cƨ@ct�@cS�@c33@b�H@b�\@b-@a��@ax�@aG�@`Ĝ@`bN@`1'@`  @_|�@_K�@_K�@_;d@_�@^�@^��@^ff@^5?@]p�@]V@\��@\z�@\j@\Z@\1@[��@[o@Zn�@Y�#@Yx�@Y7L@Y�@Y�@X��@XĜ@X�@W��@WK�@WK�@W+@V�+@VE�@U�@U�T@U�-@U`B@UV@T�/@T�@Tj@T9X@S�m@S��@S"�@R�!@R=q@Q��@Qx�@QG�@P��@P�9@P �@O|�@O�@N��@Nv�@N$�@N@M�T@M�h@M/@L�/@L��@L�@Lj@L1@K��@K33@J�H@I�#@I7L@H��@H�u@HQ�@H  @G�w@G|�@F��@F��@FE�@F5?@F$�@F{@F@E�@EV@D��@DZ@D�@C�@C@C@B��@B~�@Bn�@BM�@A�#@AX@AX@A%@@�9@@Ĝ@@�9@@�@?�@?;d@>��@>��@>5?@=�T@=�-@=�@=`B@=?}@=�@<�@<z�@<j@<j@<Z@;��@;�F@;C�@:�H@:��@:�!@:�\@:^5@:-@9��@9�#@9��@9��@9hs@9�@9%@8��@8��@8��@8��@8�9@8�u@8bN@8 �@8b@7��@7|�@7K�@7+@6��@6��@65?@5��@5p�@5O�@5/@4�j@4�@3�@3C�@3o@3@2�@2��@2�\@2^5@2-@1�^@1x�@1G�@1&�@0�9@0 �@/�@/\)@/;d@/+@/�@.ȴ@-��@-�@-�@-p�@-p�@-?}@,j@,1@+ƨ@+�@+dZ@+33@+@*�!@*^5@*=q@*-@)�@)��@)�7@)G�@)&�@)�@(��@(�`@(��@(r�@(1'@(b@'��@'��@'�w@'��@'|�@&ȴ@&ff@&V@%�@%��@%��@%p�@%p�@%p�@%O�@%�@$Z@#��@#ƨ@#��@#dZ@#33@#@"�H@"M�@"�@"J@!�@!�^@!�7@!x�@!X@!&�@!%@ �@  �@�;@��@��@�P@|�@\)@�@
=@ȴ@�+@ff@$�@�T@@�@?}@��@��@�D@j@9X@1@ƨ@�@S�@C�@"�@@�@�!@M�@=q@��@hs@�@Ĝ@��@A�@b@b@�@�@��@�w@��@|�@|�@l�@;d@+@+@�@ȴ@�R@�R@�R@ff@$�@{@�@�T@@�h@�@`B@/@�@��@��@��@z�@z�@z�@Z@I�@9X@(�@(�@1@ƨ@��@�@S�@C�@33@o@�@�@�H@��@��@��@~�@-@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��;A��;A��;A��
A���A�ȴA�ƨA�ƨA�ȴA�ȴA�ƨAʹ9A͡�A�+A���A�r�A�E�A��AȓuA�bNA�A�ƨAƺ^A�-A��TAŋDA�+AđhA�C�A×�A���A���Aº^A�{A�ƨA�M�A��A��+A��A��9A��A�"�A���A�dZA�9XA��A�VA���A��HA��A��\A�I�A�bA���A�/A�v�A��DA��A���A��;A�C�A��A��hA�(�A��A��PA�A��A��A���A�VA��A�l�A��jA���A���A���A��mA�JA�oA�VA�E�A��A�
=A���A�G�A�ZA��\A�E�A�Q�A��A���A��A��A�5?A��DA���A�33A�n�A�S�A���A�jA�=qA��A���A��7A�t�A��7A�C�A�hsA�|�A��7A�A�`BA�|�A~�Av��AshsAr��Ar��Ar1'Ao�
Am�Am�AmdZAl��Aj�9Ag�mAg
=Ae��Ad(�Ac�hAb�+A`(�A_O�A^I�A[��A[x�AZ�HAZVAW��AV  AUG�AT�RAS�AQ��AO��AN  AJ��AI�AG�mAE��AD�ADE�ACAC&�AB1A@��A<��A:�A9O�A7�A7dZA5ƨA2ĜA1��A1
=A/��A/�A.-A,�yA,E�A+��A)��A)��A)�A&�`A%hsA$�A"��A!hsA v�A M�A {AO�A��A�A"�A�\Al�AA�`A�;A�AQ�AdZA��AI�A�hAhsA�An�A��A�A��A
�/A	"�A��AbAdZAVA�DAXA�jA��A�A1AS�A �/A   @�S�@��@���@�v�@��T@��7@�;d@�E�@���@��P@��\@�@��u@�R@�{@��@�1@�K�@���@�ƨ@�t�@�@�+@�X@睲@䛦@�j@��m@�~�@�?}@�j@� �@�33@���@��H@��T@�|�@�v�@١�@���@ם�@�33@�+@���@��#@��`@��@�;d@��@ҏ\@ҧ�@�-@�&�@�Ĝ@Гu@� �@�C�@��@˝�@ʰ!@ʏ\@�=q@�O�@ȴ9@�z�@�dZ@��@��/@�1'@�"�@���@�(�@�n�@��u@�A�@�1@��
@��@�C�@��!@��@�G�@� �@��@��@�O�@�V@���@�j@��@���@��y@���@��\@�~�@�n�@�$�@��#@��^@�G�@��@���@���@�I�@�S�@�
=@���@���@�hs@�O�@���@�bN@�b@��
@�|�@�o@���@�~�@�`B@�V@��9@�b@��P@�o@���@�n�@�=q@�@�O�@��@��@��u@��@��@�t�@�l�@�o@��\@�M�@���@�7L@���@��/@��D@�I�@�I�@��;@��w@��
@���@��@��P@�\)@�S�@�"�@�o@��y@�ȴ@�~�@�=q@�{@�J@��@�@��h@�x�@�O�@�7L@�5?@�^5@�=q@�-@�-@�-@��h@�1'@�A�@�Ĝ@��9@���@��9@��u@�z�@�Q�@�b@���@�t�@�"�@�@���@�^5@�J@��#@��T@��T@��#@�@���@�p�@�`B@�7L@�/@��@��@�Q�@�  @��
@��@�|�@�S�@��@���@�^5@�=q@�@�`B@�7L@��@�%@��`@���@��j@��u@�z�@�bN@��
@�|�@�dZ@�"�@��H@���@�^5@��@���@��#@�@��@��@���@��D@�bN@��@�l�@�
=@��y@��\@�V@�J@��#@���@���@�`B@��`@�Z@�(�@���@�l�@�K�@�;d@�+@��@�
=@��H@�ȴ@���@��!@���@��+@���@�X@�?}@�7L@�/@�/@�&�@�&�@��@�V@�%@��/@��j@�z�@�A�@��
@�\)@���@��!@��\@�-@��@���@��7@�`B@�&�@���@��j@���@��D@�Z@�1'@�  @���@���@��
@���@�t�@�\)@�K�@�o@��@���@�ff@�@���@��h@�`B@�/@��@��@��`@��/@���@���@���@���@��@K�@~��@~�+@~V@~{@|�j@|(�@{��@{t�@{o@z~�@zM�@z=q@y�@xĜ@xr�@xb@w�w@w\)@v�@vȴ@v��@v��@v�+@vv�@v$�@u/@t�@t�@t(�@s��@s�F@st�@r�!@rJ@qx�@q�@q�@q%@p�@o��@o�@nȴ@nv�@n{@m�-@mO�@mV@l�@lI�@k��@k@jM�@i�@i�^@i��@i�7@i&�@i&�@i%@hĜ@h��@hbN@h1'@h  @g��@g��@g
=@f�R@fv�@e�T@e�@eO�@eV@d��@d��@d��@dj@dI�@d9X@d9X@d1@cƨ@ct�@cS�@c33@b�H@b�\@b-@a��@ax�@aG�@`Ĝ@`bN@`1'@`  @_|�@_K�@_K�@_;d@_�@^�@^��@^ff@^5?@]p�@]V@\��@\z�@\j@\Z@\1@[��@[o@Zn�@Y�#@Yx�@Y7L@Y�@Y�@X��@XĜ@X�@W��@WK�@WK�@W+@V�+@VE�@U�@U�T@U�-@U`B@UV@T�/@T�@Tj@T9X@S�m@S��@S"�@R�!@R=q@Q��@Qx�@QG�@P��@P�9@P �@O|�@O�@N��@Nv�@N$�@N@M�T@M�h@M/@L�/@L��@L�@Lj@L1@K��@K33@J�H@I�#@I7L@H��@H�u@HQ�@H  @G�w@G|�@F��@F��@FE�@F5?@F$�@F{@F@E�@EV@D��@DZ@D�@C�@C@C@B��@B~�@Bn�@BM�@A�#@AX@AX@A%@@�9@@Ĝ@@�9@@�@?�@?;d@>��@>��@>5?@=�T@=�-@=�@=`B@=?}@=�@<�@<z�@<j@<j@<Z@;��@;�F@;C�@:�H@:��@:�!@:�\@:^5@:-@9��@9�#@9��@9��@9hs@9�@9%@8��@8��@8��@8��@8�9@8�u@8bN@8 �@8b@7��@7|�@7K�@7+@6��@6��@65?@5��@5p�@5O�@5/@4�j@4�@3�@3C�@3o@3@2�@2��@2�\@2^5@2-@1�^@1x�@1G�@1&�@0�9@0 �@/�@/\)@/;d@/+@/�@.ȴ@-��@-�@-�@-p�@-p�@-?}@,j@,1@+ƨ@+�@+dZ@+33@+@*�!@*^5@*=q@*-@)�@)��@)�7@)G�@)&�@)�@(��@(�`@(��@(r�@(1'@(b@'��@'��@'�w@'��@'|�@&ȴ@&ff@&V@%�@%��@%��@%p�@%p�@%p�@%O�@%�@$Z@#��@#ƨ@#��@#dZ@#33@#@"�H@"M�@"�@"J@!�@!�^@!�7@!x�@!X@!&�@!%@ �@  �@�;@��@��@�P@|�@\)@�@
=@ȴ@�+@ff@$�@�T@@�@?}@��@��@�D@j@9X@1@ƨ@�@S�@C�@"�@@�@�!@M�@=q@��@hs@�@Ĝ@��@A�@b@b@�@�@��@�w@��@|�@|�@l�@;d@+@+@�@ȴ@�R@�R@�R@ff@$�@{@�@�T@@�h@�@`B@/@�@��@��@��@z�@z�@z�@Z@I�@9X@(�@(�@1@ƨ@��@�@S�@C�@33@o@�@�@�H@��@��@��@~�@-@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�DB
�7B
�%B
�B
{�B
�B
�'B
��B
�B
�/B
�mB
�mB
��BbB'�B%�B"�B/BK�Bn�Bq�B�=B��B�3B�BB�B�B�B{B\BJB�B)�B5?B@�BB�BC�BF�BD�BC�BC�B=qB=qB@�B<jB9XB9XB9XBN�BM�BL�BP�BW
BT�BP�BN�BG�B49B#�BuB��B�B�BB%B��B��BB�B�ZB��B��B��B�qBɺB��B�B��B{�BK�BE�BE�B>wB<jBA�B<jB�B
�B
��B
��B
�B
��B
��B
��B
��B
��B
�-B
��B
��B
�+B
}�B
n�B
aHB
[#B
9XB
$�B
bB	B	��B	�B	�B	�TB	��B	�wB	��B	ȴB	�RB	��B	�7B	��B	�JB	� B	�B	v�B	bNB	ffB	_;B	K�B	[#B	P�B	K�B	0!B	,B	:^B	6FB	+B	�B	�B	+B��B�B��B�HB�B�mB�TB�B��B�dB�hB��B��B��B��B�hBv�B�=B��B�=B�\B�7B�B�B�Bn�B�1Bz�BaHBiyBn�BaHBaHBm�By�Bv�Bo�Bo�Bp�Br�Br�Bm�BdZBm�Be`Bk�Bl�BiyBo�BjBaHBP�BC�B\)B]/B]/BXBF�BE�BZBYBW
BYBS�BK�BM�BM�BH�BM�BP�BT�BQ�BZB^5B\)B^5BYBW
BL�BR�BR�BQ�BT�BW
BS�BP�BXBYBVBW
BS�BN�B]/B]/BZBR�BO�BJ�BbNB_;BYB\)BcTBdZBbNBiyBgmB`BBVBffBhsBl�Bl�Br�Bw�Bs�Bp�Bs�Bx�Bz�B� B�B�%B�7B�1B�\B�hB�VB�7B�%B�B�hB��B��B�uB��B��B�{B�{B��B��B��B��B��B��B�B��BƨBȴBȴBȴBɺB��B��B��B�B�HB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	+B	B	bB	�B	�B	�B	�B	 �B	�B	!�B	#�B	%�B	"�B	,B	,B	,B	0!B	33B	6FB	8RB	;dB	;dB	?}B	E�B	M�B	P�B	R�B	VB	[#B	[#B	YB	YB	]/B	]/B	^5B	gmB	iyB	k�B	n�B	r�B	t�B	{�B	�B	� B	�B	�B	�B	�PB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�B	�B	�B	��B	��B	�-B	�XB	�dB	�^B	�jB	�jB	�qB	�jB	�dB	�jB	�qB	�}B	B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�5B	�5B	�/B	�TB	�`B	�`B	�`B	�fB	�`B	�`B	�fB	�fB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
	7B
B
JB
bB
bB
bB
bB
bB
bB
bB
\B
\B
VB
PB
JB
JB
DB
DB
VB
\B
hB
\B
hB
hB
hB
oB
oB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
"�B
#�B
"�B
#�B
"�B
!�B
�B
"�B
#�B
"�B
#�B
#�B
#�B
"�B
"�B
$�B
%�B
&�B
&�B
$�B
#�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
$�B
&�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
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
-B
-B
-B
-B
,B
,B
-B
-B
,B
,B
,B
-B
.B
.B
-B
.B
/B
0!B
/B
0!B
1'B
1'B
0!B
0!B
0!B
0!B
0!B
.B
1'B
1'B
2-B
33B
33B
1'B
0!B
1'B
1'B
2-B
49B
5?B
6FB
6FB
5?B
49B
49B
33B
49B
7LB
6FB
49B
6FB
7LB
8RB
8RB
7LB
7LB
8RB
9XB
8RB
9XB
8RB
9XB
9XB
:^B
:^B
:^B
<jB
<jB
<jB
<jB
;dB
<jB
=qB
>wB
>wB
>wB
@�B
@�B
?}B
?}B
@�B
A�B
@�B
?}B
?}B
?}B
>wB
?}B
=qB
?}B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
C�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
F�B
E�B
E�B
F�B
E�B
E�B
D�B
G�B
F�B
F�B
G�B
G�B
F�B
D�B
C�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
G�B
I�B
J�B
J�B
J�B
I�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
N�B
N�B
M�B
M�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
P�B
Q�B
R�B
S�B
T�B
T�B
T�B
S�B
Q�B
T�B
W
B
W
B
W
B
T�B
R�B
T�B
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
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
ZB
XB
YB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
\)B
[#B
YB
\)B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
`BB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
`BB
`BB
`BB
aHB
bNB
cTB
cTB
cTB
bNB
bNB
cTB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
e`B
ffB
ffB
ffB
ffB
ffB
e`B
e`B
ffB
ffB
dZB
ffB
gmB
hsB
hsB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
jB
l�B
l�B
l�B
l�B
m�B
n�B
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
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�DB
�DB
�DB
�^B
�^B
�JB
�JB
�JB
�JB
�DB
�RB
�tB
��B
}qB
��B
�-B
��B
�wB
�~B
��B
�sB
��BTB(�B&�B#�B0!BMBoBs3B�DB�
B��B�BB�ByB�B�BBEB+kB6+B@�BB�BC�BF�BD�BC�BC�B>(B>BA B=qB:�B:�B;JBO�BO(BNpBR BW�BU�BQ�BO�BH�B6FB&fB�B B�B!�BKB�B�qB�B�B�3B�BĜB�BӏB��B�=B�-B�B�NB� BPHBG�BF�B?}B=�BCB=�BIB
�B
��B
өB
��B
�vB
� B
ϑB
�[B
ΊB
�B
��B
��B
�#B
�B
p�B
c�B
\�B
="B
'�B
aB	�B	�TB	��B	�;B	�B	�vB	��B	�0B	�RB	��B	�fB	�~B	��B	�B	��B	�B	x�B	d�B	g�B	`�B	NpB	[�B	RB	MB	3hB	.B	;JB	7fB	,�B	"NB	�B	
	B	 OB�!B�DB��B�B�XB�&B�=B̳B��B��B��B��B��B��B��Bz^B��B�yB��B�bB��B��B�%B�%BqB��B|BdtBkQBo�BdBb�Bn�BzDBwfBp�Bp�Bq�Bs�Bs�BoOBf�Bn�Bf�Bl�Bm�Bj�BpUBk�Bb�BS�BFtB]/B^OB^BY1BIBG�BZ�BY�BW�BY�BT�BMjBN�BO(BJ#BN�BQ�BU�BR�BZ�B^�B\�B^�BY�BW�BNpBS�BS�BR�BU�BW�BT�BR BX�BY�BV�BW�BT�BPbB]dB]�BZ�BTBQNBL�Bb�B_�BZ7B]Bc�Bd�BcBi�Bg�BabBW�BgBi*BmCBmwBr�Bw�BtTBq[BtnBy�B{�B�OB�[B�YB��B�B��B��B��B�	B�EB��B��B��B��B�B�B��B�gB��B�B�NB��B��B�&B�*B�IB��B��B��B��B�B�=B�vBӏB��B��B��B��B��B� B��B��B�'B�-B��B�B�B��B�$B�8B�$B�B�B�(B�6B�qB��B	SB	zB	�B	�B	�B	�B	#B	 'B	 �B	;B	"B	$&B	&2B	#�B	,=B	,qB	,�B	0�B	3�B	6�B	8�B	;�B	;�B	?�B	E�B	M�B	QhB	S[B	VSB	[=B	[=B	YB	YB	]dB	]�B	^�B	g�B	i�B	k�B	n�B	r�B	uB	|B	� B	�4B	�'B	�GB	�mB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�5B	�5B	�5B	�IB	�B	��B	�B	�	B	�B	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�(B	�jB	�&B	�B	�$B	�?B	�eB	�eB	�WB	�]B	�jB	ބB	ݘB	�nB	�zB	�zB	�zB	�B	�zB	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�B	�B	�B	�B	�(B	�<B	�PB	�jB	�.B	�HB
[B
_B
KB
KB
	RB
	RB
	RB
	RB

rB

XB

XB
	lB
�B
~B
bB
bB
bB
bB
}B
bB
bB
vB
vB
�B
�B
~B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
	B
�B
�B
�B
G�O�B
�B
�B
�B
�B
�B
�B
 G�O�G�O�B
�B
�B
 �B
 �B
!B
"�B
#�B
#B
#�B
"�B
"G�O�B
"�B
#�B
#B
#�B
#�B
$B
# B
# B
%B
&B
'B
'G�O�B
$&B
%B
'B
'B
'8B
'B
'8B
'B
'B
'8B
'8G�O�B
'8B
)*B
*0B
*0B
*B
*B
+B
+B
+B
+B
+B
+B
+B
+B
*0B
)*B
*0B
*B
)DB
+6B
,"B
,"B
-)B
-CB
-CB
-CB
-CB
-B
-B
-)B
,"B
,"B
-)B
-CB
,"B
,"B
,WB
-CB
./B
./B
-CB
.IB
/OB
0;B
/OB
0UB
1'B
1AB
0UB
0;B
0UB
0;B
0oG�O�B
1AB
1AB
2GB
3hB
3MG�O�B
0UB
1[B
1[B
2|B
4nB
5ZB
6`B
6FB
5ZB
4TB
4TB
3hB
4nB
7LB
6zG�O�B
6`B
7�B
8lB
8lB
7fB
7fB
8�B
9rB
8lB
9rB
8�B
9rB
9�B
:�B
:�B
:�B
<�B
<�B
<�B
<�B
;�B
<�B
=�B
>�B
>�B
>�B
@�B
@�B
?�B
?�B
@�B
A�B
@�B
?�B
?�B
?�B
>�B
?�G�O�B
?�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
C�G�O�B
A�B
B�B
C�B
C�B
B�B
C�B
F�B
E�B
E�B
F�B
E�B
E�B
D�B
G�B
F�B
F�B
G�B
G�B
F�G�O�B
C�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
G�B
I�B
J�B
J�B
J�B
I�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
MB
L�B
K�B
MB
MB
L�B
K�B
LB
MB
NB
OB
N�B
N"B
N"B
O(B
QB
RB
Q�B
RB
RB
Q B
RB
RB
Q4B
R B
SB
SG�O�B
R:B
S&B
TB
UB
UB
UB
TFG�O�B
UB
W
B
W$B
W$G�O�G�O�B
U2B
W$B
W$B
X+B
W$B
W$B
W$B
W?B
X+B
X+B
XEB
X+B
X+B
XEB
Y1B
Y1B
ZQB
Y1B
Y1B
YKB
Z7B
[=B
[=B
[#B
[=B
[=B
ZQG�O�B
YKB
[=B
[=B
\CB
\CB
\CB
]/B
]/B
\CB
[qG�O�B
\CB
^OB
^OB
^OB
^OB
_VB
_VB
^jB
`\B
aHB
abB
`\B
`\B
aHB
abB
abB
`\B
`vB
`vB
a|B
bhB
cnB
cnB
cTB
bhB
bhB
cnB
b�B
bhB
cnB
b�B
cnB
cnB
cnB
cnB
d�B
dtB
d�B
ezB
ezB
f�B
f�B
ezB
f�B
ffB
f�B
f�B
f�B
ezB
ezB
f�B
f�G�O�B
f�B
g�B
h�B
h�B
i�B
j�B
j�B
jB
j�B
jB
j�B
j�B
k�B
k�B
j�B
l�B
l�B
l�B
l�B
m�B
n�B
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
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111114111111111111111141111141111111441111111111141111111111114111111111114111111111111111111111111111111111111111111111111111114111114111111111111111411111111111111111111111111111111111111411111111111111411111111111111111114111111111111111111111111111111111111111111111111111111111111111111411111114111144111111111111111111111111111411111111114111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804120037472018041200374720180412003747201806221328462018062213284620180622132846201804261707182018042617071820180426170718  JA  ARFMdecpA19c                                                                20180408093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180408003536  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180408003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180408003539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180408003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180408003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180408003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180408003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180408003540  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180408003540                      G�O�G�O�G�O�                JA  ARUP                                                                        20180408005647                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180408153914  CV  JULD            G�O�G�O�F�˳                JM  ARSQJMQC2.0                                                                 20180409000000  CF  PSAL_ADJUSTED_QCD  D� G�O�                JM  ARCAJMQC2.0                                                                 20180411153747  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180411153747  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426080718  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042846  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                