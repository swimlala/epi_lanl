CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-14T00:35:17Z creation;2018-01-14T00:35:21Z conversion to V3.1;2019-12-19T07:47:44Z update;     
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180114003517  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_200                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�DuՅ� 1   @�Dv�8�@4\��N<�dk��e��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C@\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�)Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3��D3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDzw
Dz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�D���D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�D���D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�{�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�D�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�^5A�\)A�ZA�ZA�XA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�^5A�ZA�^5A�\)A�XA�O�A�M�A�=qA�1'A�-A�-A�/A�9XA�A�A�A�A�G�Aɧ�A�n�Aǩ�A��yAƋDA�{A��/A���AŸRAōPA�E�A���Aġ�Aĝ�Aě�Aę�Aď\A�A��A�&�A���A�dZA��TA�O�A�\)A��7A��DA���A��A��A�-A�ĜA�G�A�$�A�ƨA�/A���A���A�K�A� �A�ffA���A�A��A�$�A�^5A��9A��`A�1A���A�x�A���A��uA��A���A�\)A��A�bNA�A�A��A���A��DA�bA���A���A�VA�33A���A��HA��mA��#A�VA�S�A���A�A~{A|E�A{��Ay|�Av��AvM�AuAu/At$�Ar��ApJAl5?Aj~�AgAgC�Af��Ad��Ac/Aa�FA`E�A^��A^~�A]�A[S�AY|�AW�wAV��AU;dARĜAP�AN��ALM�AK7LAJ��AJ-AIAIS�AH��AG�AE�7AB��A@��A?oA=��A<-A:��A9VA6ffA4�yA4-A3�A2�A0 �A/G�A-�-A, �A*9XA)|�A'�7A&1'A%��A$1'A#"�A!A �uA��A�A��A�A��A-A�hA-A��A�A�hAhsA33AbNA+A��AE�A�mAt�A+A��A��A��A�AbNA��A��Av�A
�A	�A(�A33A��A��A=qA|�A�A5?A��A7LA ��@�;d@�`B@��H@�l�@��7@��@��@�`B@�(�@��y@�\@�9@�R@�E�@��@�F@���@�-@�;d@��@�O�@�A�@�K�@�;d@�+@�n�@�&�@��y@�j@�;d@Ձ@�G�@�7L@�&�@���@ԣ�@��@��/@θR@͙�@���@�A�@��@�l�@�-@��/@�b@�33@ƸR@ũ�@î@�^5@��-@�V@�A�@��F@��@�$�@��#@�?}@��`@�Z@��P@��@���@���@�V@�J@�G�@�Z@���@�l�@�@�V@�{@�@��7@�V@��@��
@�|�@�33@��+@��#@���@�?}@��j@�1@���@�|�@�"�@���@�-@��@���@��@�1'@�b@���@��P@�|�@�l�@�S�@�
=@��+@���@�hs@��@���@��;@�33@�ȴ@���@�hs@���@���@��u@��D@�z�@�Q�@�1'@���@��w@�l�@�;d@�;d@�ȴ@�ff@�J@��-@��h@���@�X@���@��u@�9X@��@�ƨ@��F@��@�dZ@�S�@�S�@�S�@�C�@�o@�V@��T@���@��^@��-@��7@�?}@��@���@��/@���@���@��u@�(�@�  @��m@�  @��
@��F@��@���@�dZ@�C�@�
=@��!@���@���@��\@���@���@��\@�ff@��@���@���@�hs@�O�@��@��j@�Q�@�1'@�  @��
@��@�dZ@�S�@�C�@�
=@��H@��R@�v�@�5?@��#@��h@�O�@��@�V@�Ĝ@��u@�bN@�A�@�b@��m@��;@�ƨ@��@�K�@�
=@�o@��@���@�M�@��@��7@�p�@�X@�7L@���@��`@��9@�bN@�I�@���@�1'@��@�ƨ@���@���@�$�@��#@���@���@���@��u@�Z@�9X@�(�@�1@��@��F@�|�@�C�@�C�@�C�@���@�E�@��@���@���@���@��-@�G�@��j@��@�bN@�I�@�b@���@���@���@�S�@�;d@�@�n�@�{@��@�`B@�7L@�/@�?}@��@��`@��9@��D@�1'@��@�1@��m@���@��F@�t�@�C�@���@���@���@��+@�n�@�$�@��-@�hs@�`B@�X@�O�@�?}@�/@�&�@���@��j@��D@�Q�@� �@�@\)@~��@}�T@}��@}/@|9X@{��@{�
@{��@z��@z�\@z=q@y��@y��@yx�@yhs@yX@y7L@x��@xĜ@x�9@xA�@w��@w|�@v�y@v��@v��@v�R@v�+@u�@up�@u`B@uO�@uV@t�/@t��@s�F@s33@s@s@so@s"�@s@r�!@r-@q�#@q��@qhs@qG�@p�u@p  @o\)@nȴ@nff@n{@m�@l�@l��@k��@kt�@k"�@j�\@i�@i7L@i�@h�9@hr�@g�@g��@g|�@g+@f�y@fv�@f@ep�@eV@d�j@d��@dI�@cƨ@c��@cS�@b�H@b=q@a��@a��@a7L@`��@`Ĝ@`��@`bN@`1'@`b@_�@_l�@^�y@^ff@^5?@]�@]@]�h@]V@[�
@[��@[�@[S�@[o@Z��@ZJ@Y��@YG�@X��@X �@W�@Wl�@WK�@W+@V��@V�@V��@Vv�@Vff@V$�@U�@T�j@T9X@T1@T1@S��@S�
@S�@R~�@RJ@Q�@Q�#@Q��@Q��@Q��@Qx�@P��@PĜ@P��@P �@O�;@O|�@O;d@O�@N�y@Nȴ@N�+@M`B@L��@L�@Lj@L�@L1@K��@Ko@Jn�@JJ@I�#@Ix�@I�@H�`@H��@H�u@H�@Hr�@HA�@H �@H  @G�;@G�w@G�P@G;d@Fȴ@Fv�@FE�@E��@E��@Ep�@E`B@E/@D��@D�/@D��@Dj@D9X@Cƨ@CdZ@C@B�\@BM�@A��@A�7@A%@@Ĝ@@Ĝ@@�@?�w@?;d@?
=@>�+@>E�@>5?@>5?@>5?@>$�@>@=@<�@<I�@;ƨ@;C�@:��@:�\@:^5@:M�@:-@:J@9�@9�#@9�@8��@8��@8r�@7�;@7l�@7\)@7�@7+@7�@6�y@6�@6ȴ@6��@6V@6@5��@5�@5V@4��@4�/@4�/@4�@4Z@3�
@3��@3�@3dZ@3"�@3@2�!@2~�@2-@1�@1��@17L@1%@0��@0�9@0��@0�u@0bN@01'@/�;@/��@/l�@/\)@.��@.�R@.ff@-@-?}@,��@,��@,z�@,I�@,(�@,1@+��@+�
@+��@+t�@+"�@*��@*^5@*M�@*=q@*J@)�7@)X@)%@(Ĝ@(��@(r�@(b@'�;@'��@'�@'|�@'K�@';d@&�y@&��@&5?@&{@%@%�h@%�@%�@%V@$��@$9X@$(�@$�@#�
@#��@#t�@#"�@"�\@"n�@"^5@"-@!�#@!��@!x�@!7L@!&�@!%@ �`@ Ĝ@ �@ A�@  �@ b@�@�@\)@K�@+@�y@��@v�@�@��@p�@��@�/@�j@��@�D@z�@z�@j@(�@�
@�@S�@33@C�@33@@��@^5@J@��@�@�@�#@�^@G�@7L@7L@7L@�@��@�@Q�@A�@A�@Q�@A�@A�@Q�@ �@  @�@�P@l�@K�@��@�R@��@�+@v�@ff@ff@5?@$�@{@@�T@��@�h@�@p�@?}@?}@/@�@�D@j@9X@(�@(�@�@1@��@�m@ƨ@�F@�F@��@o@�@�H@��@^5@=q@�@J@��@��@��@��@X@7L@&�@7L@&�@��@�@bN@1'@�@�w@�@��@|�@K�@+@
=@�@ȴ@�R@�+@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�^5A�\)A�ZA�ZA�XA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�^5A�ZA�^5A�\)A�XA�O�A�M�A�=qA�1'A�-A�-A�/A�9XA�A�A�A�A�G�Aɧ�A�n�Aǩ�A��yAƋDA�{A��/A���AŸRAōPA�E�A���Aġ�Aĝ�Aě�Aę�Aď\A�A��A�&�A���A�dZA��TA�O�A�\)A��7A��DA���A��A��A�-A�ĜA�G�A�$�A�ƨA�/A���A���A�K�A� �A�ffA���A�A��A�$�A�^5A��9A��`A�1A���A�x�A���A��uA��A���A�\)A��A�bNA�A�A��A���A��DA�bA���A���A�VA�33A���A��HA��mA��#A�VA�S�A���A�A~{A|E�A{��Ay|�Av��AvM�AuAu/At$�Ar��ApJAl5?Aj~�AgAgC�Af��Ad��Ac/Aa�FA`E�A^��A^~�A]�A[S�AY|�AW�wAV��AU;dARĜAP�AN��ALM�AK7LAJ��AJ-AIAIS�AH��AG�AE�7AB��A@��A?oA=��A<-A:��A9VA6ffA4�yA4-A3�A2�A0 �A/G�A-�-A, �A*9XA)|�A'�7A&1'A%��A$1'A#"�A!A �uA��A�A��A�A��A-A�hA-A��A�A�hAhsA33AbNA+A��AE�A�mAt�A+A��A��A��A�AbNA��A��Av�A
�A	�A(�A33A��A��A=qA|�A�A5?A��A7LA ��@�;d@�`B@��H@�l�@��7@��@��@�`B@�(�@��y@�\@�9@�R@�E�@��@�F@���@�-@�;d@��@�O�@�A�@�K�@�;d@�+@�n�@�&�@��y@�j@�;d@Ձ@�G�@�7L@�&�@���@ԣ�@��@��/@θR@͙�@���@�A�@��@�l�@�-@��/@�b@�33@ƸR@ũ�@î@�^5@��-@�V@�A�@��F@��@�$�@��#@�?}@��`@�Z@��P@��@���@���@�V@�J@�G�@�Z@���@�l�@�@�V@�{@�@��7@�V@��@��
@�|�@�33@��+@��#@���@�?}@��j@�1@���@�|�@�"�@���@�-@��@���@��@�1'@�b@���@��P@�|�@�l�@�S�@�
=@��+@���@�hs@��@���@��;@�33@�ȴ@���@�hs@���@���@��u@��D@�z�@�Q�@�1'@���@��w@�l�@�;d@�;d@�ȴ@�ff@�J@��-@��h@���@�X@���@��u@�9X@��@�ƨ@��F@��@�dZ@�S�@�S�@�S�@�C�@�o@�V@��T@���@��^@��-@��7@�?}@��@���@��/@���@���@��u@�(�@�  @��m@�  @��
@��F@��@���@�dZ@�C�@�
=@��!@���@���@��\@���@���@��\@�ff@��@���@���@�hs@�O�@��@��j@�Q�@�1'@�  @��
@��@�dZ@�S�@�C�@�
=@��H@��R@�v�@�5?@��#@��h@�O�@��@�V@�Ĝ@��u@�bN@�A�@�b@��m@��;@�ƨ@��@�K�@�
=@�o@��@���@�M�@��@��7@�p�@�X@�7L@���@��`@��9@�bN@�I�@���@�1'@��@�ƨ@���@���@�$�@��#@���@���@���@��u@�Z@�9X@�(�@�1@��@��F@�|�@�C�@�C�@�C�@���@�E�@��@���@���@���@��-@�G�@��j@��@�bN@�I�@�b@���@���@���@�S�@�;d@�@�n�@�{@��@�`B@�7L@�/@�?}@��@��`@��9@��D@�1'@��@�1@��m@���@��F@�t�@�C�@���@���@���@��+@�n�@�$�@��-@�hs@�`B@�X@�O�@�?}@�/@�&�@���@��j@��D@�Q�@� �@�@\)@~��@}�T@}��@}/@|9X@{��@{�
@{��@z��@z�\@z=q@y��@y��@yx�@yhs@yX@y7L@x��@xĜ@x�9@xA�@w��@w|�@v�y@v��@v��@v�R@v�+@u�@up�@u`B@uO�@uV@t�/@t��@s�F@s33@s@s@so@s"�@s@r�!@r-@q�#@q��@qhs@qG�@p�u@p  @o\)@nȴ@nff@n{@m�@l�@l��@k��@kt�@k"�@j�\@i�@i7L@i�@h�9@hr�@g�@g��@g|�@g+@f�y@fv�@f@ep�@eV@d�j@d��@dI�@cƨ@c��@cS�@b�H@b=q@a��@a��@a7L@`��@`Ĝ@`��@`bN@`1'@`b@_�@_l�@^�y@^ff@^5?@]�@]@]�h@]V@[�
@[��@[�@[S�@[o@Z��@ZJ@Y��@YG�@X��@X �@W�@Wl�@WK�@W+@V��@V�@V��@Vv�@Vff@V$�@U�@T�j@T9X@T1@T1@S��@S�
@S�@R~�@RJ@Q�@Q�#@Q��@Q��@Q��@Qx�@P��@PĜ@P��@P �@O�;@O|�@O;d@O�@N�y@Nȴ@N�+@M`B@L��@L�@Lj@L�@L1@K��@Ko@Jn�@JJ@I�#@Ix�@I�@H�`@H��@H�u@H�@Hr�@HA�@H �@H  @G�;@G�w@G�P@G;d@Fȴ@Fv�@FE�@E��@E��@Ep�@E`B@E/@D��@D�/@D��@Dj@D9X@Cƨ@CdZ@C@B�\@BM�@A��@A�7@A%@@Ĝ@@Ĝ@@�@?�w@?;d@?
=@>�+@>E�@>5?@>5?@>5?@>$�@>@=@<�@<I�@;ƨ@;C�@:��@:�\@:^5@:M�@:-@:J@9�@9�#@9�@8��@8��@8r�@7�;@7l�@7\)@7�@7+@7�@6�y@6�@6ȴ@6��@6V@6@5��@5�@5V@4��@4�/@4�/@4�@4Z@3�
@3��@3�@3dZ@3"�@3@2�!@2~�@2-@1�@1��@17L@1%@0��@0�9@0��@0�u@0bN@01'@/�;@/��@/l�@/\)@.��@.�R@.ff@-@-?}@,��@,��@,z�@,I�@,(�@,1@+��@+�
@+��@+t�@+"�@*��@*^5@*M�@*=q@*J@)�7@)X@)%@(Ĝ@(��@(r�@(b@'�;@'��@'�@'|�@'K�@';d@&�y@&��@&5?@&{@%@%�h@%�@%�@%V@$��@$9X@$(�@$�@#�
@#��@#t�@#"�@"�\@"n�@"^5@"-@!�#@!��@!x�@!7L@!&�@!%@ �`@ Ĝ@ �@ A�@  �@ b@�@�@\)@K�@+@�y@��@v�@�@��@p�@��@�/@�j@��@�D@z�@z�@j@(�@�
@�@S�@33@C�@33@@��@^5@J@��@�@�@�#@�^@G�@7L@7L@7L@�@��@�@Q�@A�@A�@Q�@A�@A�@Q�@ �@  @�@�P@l�@K�@��@�R@��@�+@v�@ff@ff@5?@$�@{@@�T@��@�h@�@p�@?}@?}@/@�@�D@j@9X@(�@(�@�@1@��@�m@ƨ@�F@�F@��@o@�@�H@��@^5@=q@�@J@��@��@��@��@X@7L@&�@7L@&�@��@�@bN@1'@�@�w@�@��@|�@K�@+@
=@�@ȴ@�R@�+@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B.B.B.B.B.B.B.B.B.B.B.B.B.B.B-B-B-B-B-B-B-B-B-B-B.B/B.B/B/B0!B0!B2-B9XB>wBD�BP�Bx�B�B�=B�VB��B��B�9BB(�BB�BJ�BL�BH�BE�BF�BE�BI�BI�BF�B?}B0!BB  B�BŢB�yB��B�TB�B�B��BB��BBhBJB��B��B��B�B�sB��B��B��B�B�TBĜBĜB�B�B�qB�TB�B�B�B��B�{B�%B]/BZBS�B&�BJB
�B
�dB
�1B
�B
_;B
P�B
ffB
[#B
F�B
�B
C�B
H�B
=qB
#�B
+B
$�B
#�B
%�B
uB
B
uB
hB
1B	��B	�B	��B	�B	�RB	��B	�'B	��B	��B	�+B	�+B	y�B	s�B	u�B	k�B	Q�B	A�B	>wB	7LB	+B	�B	1B	PB��B	1B	PB	DB	%B	B��B�B�/BǮB��BǮBĜB�wB�LB�B��B��B��B��B��B�VB�hB�=B�By�B�B{�By�B�Bu�Bw�Bp�Bs�Bs�Bt�BbNBs�Br�Br�Bm�BhsBs�By�Bw�Bt�Bq�BiyBaHBn�Bo�Bo�Bn�Bp�Bo�Bk�BcTBN�B<jBXBC�BO�BJ�BN�BK�BQ�B_;B\)BT�BJ�B2-BM�BP�BS�BQ�BF�BB�B=qB7LB?}BF�BK�BI�BL�BM�BQ�BJ�BF�BT�BO�BN�BR�BM�BI�BS�B[#BZB\)BdZBaHBZBS�BN�BQ�BaHBaHBp�Bp�Bo�Bk�BgmB]/B_;BdZBs�Bz�B~�B�B|�Bz�B}�B�%B�+B�DB�+B�B�JB��B��B��B��B��B��B��B��B��B��B�B�!B�LB�LB�LB�LB�?B�RB�wBĜBŢBŢB��B��B��B��B��B��B�B�)B�)B�BB�sB�sB�B�B��B��B��B��B	B	1B	DB		7B	bB	�B	�B	�B	!�B	"�B	!�B	"�B	"�B	&�B	)�B	/B	/B	2-B	6FB	;dB	A�B	A�B	G�B	N�B	VB	VB	XB	XB	ZB	^5B	cTB	iyB	l�B	p�B	o�B	o�B	q�B	r�B	u�B	x�B	w�B	v�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�+B	�1B	�=B	�JB	�PB	�DB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�FB	�XB	�jB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�)B	�/B	�;B	�BB	�HB	�NB	�TB	�`B	�ZB	�`B	�fB	�yB	�sB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
  B
B
B
B
%B
1B
	7B
1B
1B
+B
+B
%B
	7B
1B
1B

=B

=B
	7B
	7B
DB
PB
PB
PB
PB
JB
JB
VB
oB
oB
hB
hB
hB
hB
bB
\B
bB
bB
hB
bB
hB
hB
oB
{B
{B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
'�B
&�B
&�B
&�B
&�B
(�B
'�B
'�B
&�B
&�B
&�B
(�B
)�B
)�B
)�B
(�B
+B
)�B
)�B
+B
,B
-B
,B
.B
.B
/B
.B
/B
/B
/B
-B
.B
.B
0!B
0!B
0!B
0!B
/B
-B
2-B
33B
2-B
2-B
1'B
0!B
1'B
33B
2-B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
5?B
49B
5?B
6FB
9XB
:^B
9XB
9XB
7LB
6FB
9XB
;dB
<jB
<jB
<jB
;dB
:^B
:^B
;dB
;dB
:^B
<jB
<jB
=qB
=qB
=qB
=qB
<jB
9XB
=qB
>wB
?}B
?}B
?}B
>wB
=qB
?}B
@�B
B�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
H�B
F�B
I�B
J�B
J�B
J�B
L�B
L�B
L�B
L�B
J�B
J�B
H�B
I�B
J�B
K�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
M�B
O�B
M�B
M�B
N�B
P�B
O�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
P�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
W
B
W
B
W
B
VB
VB
VB
VB
VB
W
B
W
B
VB
VB
VB
T�B
VB
XB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
]/B
^5B
^5B
]/B
\)B
]/B
]/B
^5B
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
_;B
_;B
`BB
`BB
aHB
aHB
`BB
aHB
`BB
aHB
cTB
cTB
bNB
bNB
bNB
bNB
aHB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
ffB
ffB
e`B
ffB
ffB
e`B
ffB
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
hsB
iyB
jB
jB
k�B
jB
iyB
iyB
iyB
jB
l�B
l�B
l�B
l�B
k�B
jB
l�B
m�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�B
n�B
n�B
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
p�B
n�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
s�B
r�B
r�B
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
t�B
t�B
t�B
u�B
u�B
u�B
t�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B.B.B.B.B.B.B.B.B.B.B.B.B.B.B-B-B-B-B-B-B-B-B-B-B.B/B.B/B/5B0;B0;B2GB9XB>wBD�BQBx�B�3B��B�B�B�B��B�B)�BB�BJ�BMBI7BFYBGzBE�BI�BI�BF�B@OB2�G�O�B�B�B��B��B��B�`B�wB�B��BaB;B�B�BB�JB�kB��B�|B��B��B��B��B�}B�`BȚB��B��B��B��B�B�B��B�KG�O�B�B��Ba�B[�BW
G�O�B�B
�aB
�iB
��B
��B
c�B
S�B
gB
\�B
I�B
 \B
E�B
J�B
?�B
'�B
-)B
'B
%�B
'B
B
�B
,B
:B
	7B	��B	��B	ңB	��B	�^B	��B	��B	�0B	��B	��B	�B	{�B	uZB	v�B	m�G�O�B	C�B	@�B	8�B	-)B	�B	)B	�B��B		lB	B	�B	�B	�B�0B��BߤB�^B�<BɺBƎB��B�rB��B��B��B�B�QB��B��B��B�dB�9B|PB�gB~BB{B��BxByXBr|Bu%Bt�Bu�Bd�Bt�Bs�BshBn�BjBt9BzBx8Bu%BrGBj�Bb�Bo5Bp!Bp;Bo5BqBpBlWBd�G�O�B?�BYBF?BQ�BL�BP}BNBS&B_�B\�BVBM6G�O�BN�BQ�BT�BR�BHKBDB?}B9�B@�BG�BL�BJ�BM�BN�BRoBL0BG�BUgBP�BO�BS�BN�BK^BT�B[�BZ�B\�BdtBa�BZ�BU2BP�BS�Bb4BbNBp�Bp�Bo�Bk�Bh$B^�B`�Be�Bt�B{dBcB�[B}�B{�B~�B��B��B��B�1B��B�PB�$B�B�=B�/B�VB�NB�XB�mB�KB�B��B��B�fB��B��B��B��B�	B��B��B�B�%B��B�B�0B�VB�NBӏBچB�xBܬB��B��B��B��B�/B��B�B�0B�VB	aB	fB	�B		�B	�B	�B	�B	�B	!�B	#B	"B	# B	#nB	'RB	*eB	/iB	/�B	2�B	6�B	;�B	BB	BB	HB	O(B	VB	VB	X+B	XEB	ZQB	^jB	c�B	i�B	l�B	p�B	pB	o�B	q�B	r�B	u�B	x�B	xB	w2B	|6B	|6B	}"B	~B	B	�4B	�3B	�EB	�KB	�rB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�IB	�5B	�/B	�CB	�5B	�UB	�vB	�`B	�rB	�jB	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�B	�&B	�B	�B	�&B	�B	�2B	�2B	�9B	�YB	�KB	�QB	�]B	�IB	�]B	�dB	�pB	�vB	�|B	�hB	�nB	�zB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	�CB	��B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�+B	�%B	�	B	�B	�B	�B	�(B	�<B	�PB	�.B
 B
AB
;B
'B
GB
GB
[B
-B
AB
 �B
GB
9B
uB
?B
1B
	7B
fB
fB
_B
_B
tB
	RB
KB
KB

XB

XB
	�B
	lB
xB
�B
jB
jB
jB
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 �B
!�B
!�B
 �B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
!�B
!�B
!B
!B
!�B
!�G�O�B
$B
$&B
$@B
%B
(
B
'B
'B
'B
'B
)B
(
B
($B
'B
'B
'B
)*B
*0B
*B
*B
)*B
+B
*0B
*0B
+6B
,"B
-CB
,=B
./B
./B
/OB
./B
/OB
/OB
/5G�O�B
.cB
.cB
0;B
0;B
0UB
0UB
/OB
-wB
2GB
3hB
2GB
2GB
1[B
0oB
1vB
3MB
2|B
3hB
4nB
5ZB
6`B
6`B
6zB
6zB
6`B
6zB
7fG�O�B
4�B
5�B
6zB
9rB
:^B
9rB
9rG�O�B
6�B
9rB
;dB
<jB
<�B
<jB
;B
:�B
:�B
;B
;B
:�B
<�B
<�B
=�B
=�B
=�B
=�B
<�G�O�B
=�B
>�B
?�B
?�B
?�B
>�B
=�B
?�B
@�B
B�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
H�G�O�B
I�B
J�B
J�B
J�B
L�B
L�B
L�B
L�G�O�B
KG�O�B
J	B
KB
K�B
MB
M�B
N�B
N�B
N�B
OB
N�B
M�B
MB
M�B
O�G�O�B
N"B
OB
P�B
O�B
Q�B
RB
RB
RB
R�B
R B
RB
QB
QB
Q4B
R�B
R�B
SB
R�B
SB
R B
R:B
TB
U2B
UB
UB
UB
UB
U2B
U2B
VB
U2B
VB
V9B
W$B
W$B
W$B
VB
VB
VB
VB
VB
W$B
W$B
VSB
VB
V9B
UMB
V9B
XEB
Z7B
[=B
[=B
\CB
\CB
\CB
\CB
\CB
\CB
[WB
[WB
]dB
^5B
^jB
]IB
\xB
]IB
]IB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
_VB
_VB
_VB
_VB
_pB
`\B
`\B
abB
a|B
`\B
abB
`vB
abB
cnB
cnB
bhB
b�B
bhB
bhB
a�B
c�B
dtB
dtB
cnB
d�B
dtB
d�B
e`B
ezB
ezB
ezB
e�B
ezB
ezB
ffB
f�B
ezB
ezB
ffB
f�B
e�B
f�B
f�B
e�B
f�B
g�B
g�B
h�B
i�B
i�B
iyB
iyB
iyB
i�B
h�B
h�B
i�B
j�B
j�B
k�B
j�B
i�B
i�B
i�B
j�B
l�B
l�B
l�B
l�B
k�B
j�B
l�B
m�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�B
n�B
n�B
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
p�G�O�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
s�B
r�B
r�B
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
t�B
t�B
t�B
u�B
u�B
u�B
t�B
v�B
v�1111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111411111411111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111411111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111411111111111111111111111111141111111411111111111111111114111111111111111111111111111111111111111111111111411111111414111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801180034462018011800344620180118003446201806221324512018062213245120180622132451201804050728122018040507281220180405072812  JA  ARFMdecpA19c                                                                20180114093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180114003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180114003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180114003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180114003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180114003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180114003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180114003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180114003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180114003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180114005517                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180114153638  CV  JULD            G�O�G�O�F�#�                JM  ARSQJMQC2.0                                                                 20180115000000  CF  PSAL_ADJUSTED_QCB�  D� G�O�                JM  ARCAJMQC2.0                                                                 20180117153446  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180117153446  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222812  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042451  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                