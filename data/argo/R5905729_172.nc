CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-01-11T08:01:34Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230111080134  20230111080134  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�T�K�1   @�T��A@*$Z�1�d}hr� �1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�z�A��B�
B�
B�
B�
B'�
B/�
B7�
B@=pBG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
Bp�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!wD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D���D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�A�D�~�D뾸D���D�>�D��D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A�x�A�x�A�x�A�x�A�x�A�r�A�r�A�`BA�S�A�O�A�O�A�O�A�M�A�M�A�K�A�I�A�I�A�I�A�C�A�?}A�9XA�33A�/A�-A�+A�$�A�$�A�$�A�oA�bA�A� �AƗ�A��A��A��RA�\)A�$�A��jA��A���A���A�5?A�/A��#A�\)A�33A�O�A��!A��yA��`A��#A�1A���A��A�A�ĜA�;dA��A�ĜA��yA��A�K�A�O�A�XA��
A���A���A��-A�VA�VA�/A�ƨA��;A��FA���A~Q�A|�Az��Aw;dAp��AnȴAm��Ak�
Af1A\�AQ��AMXAIK�AG/ADZABVA?�PA<�DA;�hA:��A8�A7C�A5�wA4A�A2��A2�\A1�#A17LA-x�A*ȴA)�FA)�A)G�A(E�A'x�A'�A&�/A&�uA%�wA%l�A$�A!7LAp�AdZA�^A7LA�A��At�A�A �A��Ar�A+Ax�A?}A��AȴA�Av�A$�A��A�PA
=A��AO�A��Az�AE�A �A�A�mA�-Ax�A|�A�A  A��AA��A�A|�A\)A7LA
=A�A��A1AXA+A�A�PA\)A�A�yA�A{A|�A
�jA	��A	�;A	ƨA	�FA	�7A	/A��A��AoA��A�A^5A��Ax�A~�A�;A`BA ��A ��A �RA �9A �!A �DA ^5A $�@�dZ@��@�5?@��h@�&�@���@��w@��@�"�@���@�5?@��@��9@�C�@�{@�X@�Ĝ@�1'@�@�33@��@�v�@�h@�j@��
@�"�@�E�@�O�@�u@�P@�$�@�O�@�z�@�b@�+@��@�%@�%@��@��/@�Ĝ@�z�@��@�@��@�ȴ@�v�@���@���@���@��D@�A�@���@�ȴ@�E�@�`B@��/@܃@�dZ@ڏ\@�V@�-@��@ٲ-@��`@�1'@ו�@�o@��@��@���@�~�@�5?@Ձ@Ԭ@�dZ@�=q@щ7@�x�@�X@�G�@�O�@�/@�A�@ϥ�@��H@��@ΰ!@�J@̓u@�;d@ʟ�@�5?@ɡ�@�p�@�G�@�G�@�&�@���@ȴ9@�r�@�A�@���@�C�@�
=@�ff@�G�@���@���@�1'@�ƨ@�|�@��@\@��T@���@���@�V@��@��@���@�X@�/@��D@�+@�n�@�V@�5?@���@�|�@��m@��@��/@���@�Ĝ@��u@�Z@� �@�  @���@�K�@�+@�\)@��@�V@��7@��@��@�bN@��@�\)@�33@�
=@��\@��@���@�x�@�O�@�7L@���@��9@�z�@�Z@�1'@��w@�|�@�C�@���@��@���@�v�@�n�@�ff@�M�@�@�x�@���@�r�@�I�@�(�@���@���@���@�;d@��@��+@�E�@��@��@�V@���@�r�@�b@�t�@��y@��@���@��\@���@�`B@�7L@��@���@���@�bN@�1'@���@���@�ȴ@�v�@�{@��^@�`B@��`@�j@���@���@�t�@��H@��+@�M�@��@��@��-@�`B@�%@���@��@�Ĝ@�j@�1@�l�@�;d@�+@�+@�"�@�@�~�@�v�@�=q@�@��@�Ĝ@��u@�b@��F@��P@�|�@�l�@�S�@��R@��@���@��@��@���@��j@�z�@�(�@��F@�l�@��@���@�5?@���@�x�@�7L@��@��`@���@�1'@��m@���@�\)@��\@�-@�{@���@��T@�@��@�hs@�/@�%@��j@�Z@�9X@�  @�o@��R@�E�@�@�O�@�&�@���@��`@���@�1'@�1@�ƨ@�dZ@�;d@��@���@��\@�5?@���@��#@���@���@�G�@��/@��@�9X@���@�+@��y@��+@�n�@�^5@�5?@��@���@�p�@�?}@���@��D@�z�@�b@�ƨ@��F@���@�\)@��@���@��@���@��7@�p�@��@��u@�bN@�1'@�@~��@}�@}�T@}��@}�@|�/@|j@{��@{@z��@zn�@z^5@z�@y�^@yhs@x��@xA�@w��@w;d@w
=@v�@v��@vV@v@u�@u��@u�@u?}@t�j@s�F@s33@r�!@rM�@qx�@p�u@o�;@o�w@oK�@n�y@m�@m?}@mV@l�j@kƨ@j��@j=q@jJ@jJ@i��@i��@i%@h�9@hbN@hb@g�w@g��@g|�@g|�@gl�@g;d@f�R@f�+@fV@f@eO�@d�j@dI�@c�F@c"�@b�@b��@b�\@bn�@b-@a��@a&�@`�u@`b@_��@_�@_;d@^��@^��@^ff@]�T@]O�@\��@\��@[��@[�
@[�
@[�
@[ƨ@[��@[S�@["�@Z�H@Z=q@Y�^@Yhs@Y&�@XĜ@X  @W��@W|�@V�y@V5?@V@U@U��@U`B@T��@TZ@S��@S33@R�@R�!@Rn�@R^5@R�@Qx�@Q7L@Q�@P��@O��@OK�@N�@N$�@M�T@M�-@Mp�@M�@MV@MV@L��@L�/@L�D@LI�@L9X@L�@Kƨ@KC�@J�@J��@J�!@J�\@J~�@JM�@I��@IX@H�@H �@G�@G|�@G;d@Fȴ@Fv�@F$�@E�T@Ep�@Dz�@D9X@C�@CC�@Co@C@B�@B�@B��@B^5@B=q@B�@A��@A��@A�#@A��@A��@A��@AX@@��@@r�@@ �@?��@?K�@>�y@>��@>v�@>ff@>E�@>5?@=��@=�@=V@<��@<�@<��@<j@<9X@;S�@:�\@:n�@:n�@:M�@:-@:J@9��@9x�@9G�@8��@8�9@8�9@8A�@7+@6�R@6��@6V@6$�@5�T@5O�@5?}@4��@3��@2�H@2��@2�\@2~�@2�@1��@1��@1�^@1��@1x�@1G�@1�@0�`@0��@0Ĝ@0A�@0  @/�@/�;@/K�@.�R@.{@-�@-O�@-�@,�@,�j@,��@,�D@,j@,Z@,Z@,Z@,9X@,(�@,�@,1@+�F@+��@+�@+S�@+@*��@*��@*n�@*^5@*�@)�#@)�7@)X@)%@(��@(��@(�u@(r�@(bN@'�@'|�@'+@&�@&��@&��@&ff@&E�@&@%�T@%�-@%�@%O�@%V@$z�@#��@#��@#S�@#C�@#33@#o@"��@"�\@"�\@"�\@"~�@"=q@!�#@!��@!hs@ �u@ Q�@��@�@ȴ@��@ff@{@��@?}@�@�@�D@I�@�@�m@�F@��@S�@"�@o@�H@��@�!@~�@=q@J@�7@7L@&�@�@�`@�u@Q�@ �@�@�w@�P@l�@;d@�y@�R@v�@v�@ff@ff@5?@{@@@��@�@?}@�@z�@(�@1@�
@ƨ@�F@��@��@�@dZ@C�@"�@�H@��@��@��@M�@�@�@J@J@J@��@��@�@��@x�@X@&�@��@Q�@1'@  @  @�@�@��@�w@�@\)@
=@
=@��@�y@�R@�+@v�@v�@ff@E�@@�T@��@��@�-@��@�h@p�@?}@�@�/@��@�@��@z�@9X@(�@��@��@�@t�@dZ@C�@33@o@
�!@
�!@
��@
~�@
^5@
M�@
=q@
�@
J@
J@	��@	��@	��@	�@	�^@	��@	��@	hs@	hs@	hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�x�A�x�A�x�A�x�A�x�A�x�A�r�A�r�A�`BA�S�A�O�A�O�A�O�A�M�A�M�A�K�A�I�A�I�A�I�A�C�A�?}A�9XA�33A�/A�-A�+A�$�A�$�A�$�A�oA�bA�A� �AƗ�A��A��A��RA�\)A�$�A��jA��A���A���A�5?A�/A��#A�\)A�33A�O�A��!A��yA��`A��#A�1A���A��A�A�ĜA�;dA��A�ĜA��yA��A�K�A�O�A�XA��
A���A���A��-A�VA�VA�/A�ƨA��;A��FA���A~Q�A|�Az��Aw;dAp��AnȴAm��Ak�
Af1A\�AQ��AMXAIK�AG/ADZABVA?�PA<�DA;�hA:��A8�A7C�A5�wA4A�A2��A2�\A1�#A17LA-x�A*ȴA)�FA)�A)G�A(E�A'x�A'�A&�/A&�uA%�wA%l�A$�A!7LAp�AdZA�^A7LA�A��At�A�A �A��Ar�A+Ax�A?}A��AȴA�Av�A$�A��A�PA
=A��AO�A��Az�AE�A �A�A�mA�-Ax�A|�A�A  A��AA��A�A|�A\)A7LA
=A�A��A1AXA+A�A�PA\)A�A�yA�A{A|�A
�jA	��A	�;A	ƨA	�FA	�7A	/A��A��AoA��A�A^5A��Ax�A~�A�;A`BA ��A ��A �RA �9A �!A �DA ^5A $�@�dZ@��@�5?@��h@�&�@���@��w@��@�"�@���@�5?@��@��9@�C�@�{@�X@�Ĝ@�1'@�@�33@��@�v�@�h@�j@��
@�"�@�E�@�O�@�u@�P@�$�@�O�@�z�@�b@�+@��@�%@�%@��@��/@�Ĝ@�z�@��@�@��@�ȴ@�v�@���@���@���@��D@�A�@���@�ȴ@�E�@�`B@��/@܃@�dZ@ڏ\@�V@�-@��@ٲ-@��`@�1'@ו�@�o@��@��@���@�~�@�5?@Ձ@Ԭ@�dZ@�=q@щ7@�x�@�X@�G�@�O�@�/@�A�@ϥ�@��H@��@ΰ!@�J@̓u@�;d@ʟ�@�5?@ɡ�@�p�@�G�@�G�@�&�@���@ȴ9@�r�@�A�@���@�C�@�
=@�ff@�G�@���@���@�1'@�ƨ@�|�@��@\@��T@���@���@�V@��@��@���@�X@�/@��D@�+@�n�@�V@�5?@���@�|�@��m@��@��/@���@�Ĝ@��u@�Z@� �@�  @���@�K�@�+@�\)@��@�V@��7@��@��@�bN@��@�\)@�33@�
=@��\@��@���@�x�@�O�@�7L@���@��9@�z�@�Z@�1'@��w@�|�@�C�@���@��@���@�v�@�n�@�ff@�M�@�@�x�@���@�r�@�I�@�(�@���@���@���@�;d@��@��+@�E�@��@��@�V@���@�r�@�b@�t�@��y@��@���@��\@���@�`B@�7L@��@���@���@�bN@�1'@���@���@�ȴ@�v�@�{@��^@�`B@��`@�j@���@���@�t�@��H@��+@�M�@��@��@��-@�`B@�%@���@��@�Ĝ@�j@�1@�l�@�;d@�+@�+@�"�@�@�~�@�v�@�=q@�@��@�Ĝ@��u@�b@��F@��P@�|�@�l�@�S�@��R@��@���@��@��@���@��j@�z�@�(�@��F@�l�@��@���@�5?@���@�x�@�7L@��@��`@���@�1'@��m@���@�\)@��\@�-@�{@���@��T@�@��@�hs@�/@�%@��j@�Z@�9X@�  @�o@��R@�E�@�@�O�@�&�@���@��`@���@�1'@�1@�ƨ@�dZ@�;d@��@���@��\@�5?@���@��#@���@���@�G�@��/@��@�9X@���@�+@��y@��+@�n�@�^5@�5?@��@���@�p�@�?}@���@��D@�z�@�b@�ƨ@��F@���@�\)@��@���@��@���@��7@�p�@��@��u@�bN@�1'@�@~��@}�@}�T@}��@}�@|�/@|j@{��@{@z��@zn�@z^5@z�@y�^@yhs@x��@xA�@w��@w;d@w
=@v�@v��@vV@v@u�@u��@u�@u?}@t�j@s�F@s33@r�!@rM�@qx�@p�u@o�;@o�w@oK�@n�y@m�@m?}@mV@l�j@kƨ@j��@j=q@jJ@jJ@i��@i��@i%@h�9@hbN@hb@g�w@g��@g|�@g|�@gl�@g;d@f�R@f�+@fV@f@eO�@d�j@dI�@c�F@c"�@b�@b��@b�\@bn�@b-@a��@a&�@`�u@`b@_��@_�@_;d@^��@^��@^ff@]�T@]O�@\��@\��@[��@[�
@[�
@[�
@[ƨ@[��@[S�@["�@Z�H@Z=q@Y�^@Yhs@Y&�@XĜ@X  @W��@W|�@V�y@V5?@V@U@U��@U`B@T��@TZ@S��@S33@R�@R�!@Rn�@R^5@R�@Qx�@Q7L@Q�@P��@O��@OK�@N�@N$�@M�T@M�-@Mp�@M�@MV@MV@L��@L�/@L�D@LI�@L9X@L�@Kƨ@KC�@J�@J��@J�!@J�\@J~�@JM�@I��@IX@H�@H �@G�@G|�@G;d@Fȴ@Fv�@F$�@E�T@Ep�@Dz�@D9X@C�@CC�@Co@C@B�@B�@B��@B^5@B=q@B�@A��@A��@A�#@A��@A��@A��@AX@@��@@r�@@ �@?��@?K�@>�y@>��@>v�@>ff@>E�@>5?@=��@=�@=V@<��@<�@<��@<j@<9X@;S�@:�\@:n�@:n�@:M�@:-@:J@9��@9x�@9G�@8��@8�9@8�9@8A�@7+@6�R@6��@6V@6$�@5�T@5O�@5?}@4��@3��@2�H@2��@2�\@2~�@2�@1��@1��@1�^@1��@1x�@1G�@1�@0�`@0��@0Ĝ@0A�@0  @/�@/�;@/K�@.�R@.{@-�@-O�@-�@,�@,�j@,��@,�D@,j@,Z@,Z@,Z@,9X@,(�@,�@,1@+�F@+��@+�@+S�@+@*��@*��@*n�@*^5@*�@)�#@)�7@)X@)%@(��@(��@(�u@(r�@(bN@'�@'|�@'+@&�@&��@&��@&ff@&E�@&@%�T@%�-@%�@%O�@%V@$z�@#��@#��@#S�@#C�@#33@#o@"��@"�\@"�\@"�\@"~�@"=q@!�#@!��@!hs@ �u@ Q�@��@�@ȴ@��@ff@{@��@?}@�@�@�D@I�@�@�m@�F@��@S�@"�@o@�H@��@�!@~�@=q@J@�7@7L@&�@�@�`@�u@Q�@ �@�@�w@�P@l�@;d@�y@�R@v�@v�@ff@ff@5?@{@@@��@�@?}@�@z�@(�@1@�
@ƨ@�F@��@��@�@dZ@C�@"�@�H@��@��@��@M�@�@�@J@J@J@��@��@�@��@x�@X@&�@��@Q�@1'@  @  @�@�@��@�w@�@\)@
=@
=@��@�y@�R@�+@v�@v�@ff@E�@@�T@��@��@�-@��@�h@p�@?}@�@�/@��@�@��@z�@9X@(�@��@��@�@t�@dZ@C�@33@o@
�!@
�!@
��@
~�@
^5@
M�@
=q@
�@
J@
J@	��@	��@	��@	�@	�^@	��@	��@	hs@	hs@	hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	�B
��BYB�{B�{Bw�BbNBɺB�?B�B�'B�wB�dB�B�dB�/B�/B�B�B��BBoBhB  B�yB�B��BƨB�!B��B�B]/BG�B�B
�#B
�qB
�B
��B
�1B
t�B
S�B
=qB
0!B
1B
%B

=B	��B	�B	�fB	��B	��B	�9B	�!B	��B	v�B	-B	B�B�sB�B�5B�#B��BɺB�5B�;B�
B��B��B�#B�sB��B�B�B�yB�B	B	\B	uB	�B	�B	&�B	+B	.B	/B	I�B	K�B	B�B	cTB	y�B	�B	�B	y�B	k�B	�%B	�VB	�{B	�%B	��B	�'B	�jB	B	�wB	��B	ÖB	��B	�B	�B	�B	�
B	��B	�TB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
	7B
PB
hB
uB
�B
�B
�B
�B
hB
{B
JB
hB
{B
{B
{B
oB
VB
PB
JB
PB
�B
�B
�B
�B
{B
uB
uB
uB
�B
hB
\B
�B
�B
bB
bB
oB
oB
uB
{B
{B
{B
uB
oB
hB
bB
hB
hB
oB
oB
hB
\B
bB
\B
VB
VB
DB
DB
DB

=B
JB
PB
PB
JB
PB
PB
DB
+B
%B
+B
B
B
B
B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B	��B
B
  B
B
  B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
  B	��B	��B
  B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
1B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
!�B
!�B
�B
�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
!�B
 �B
 �B
!�B
#�B
#�B
#�B
"�B
!�B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
!�B
"�B
"�B
!�B
"�B
%�B
%�B
#�B
 �B
%�B
&�B
%�B
&�B
&�B
%�B
%�B
#�B
$�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
)�B
(�B
)�B
+B
,B
,B
+B
+B
+B
-B
-B
,B
+B
+B
+B
.B
/B
/B
/B
-B
,B
.B
-B
+B
,B
-B
.B
.B
/B
1'B
1'B
1'B
0!B
.B
/B
1'B
1'B
/B
0!B
2-B
1'B
0!B
0!B
1'B
2-B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
49B
5?B
5?B
5?B
33B
7LB
9XB
9XB
9XB
9XB
8RB
9XB
8RB
8RB
8RB
7LB
8RB
7LB
49B
9XB
8RB
9XB
:^B
<jB
<jB
<jB
;dB
;dB
<jB
=qB
<jB
>wB
>wB
=qB
>wB
>wB
@�B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
?}B
>wB
A�B
A�B
B�B
B�B
A�B
A�B
B�B
B�B
B�B
A�B
A�B
C�B
B�B
C�B
E�B
D�B
C�B
B�B
B�B
A�B
D�B
E�B
E�B
C�B
D�B
F�B
F�B
E�B
E�B
F�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
L�B
K�B
N�B
N�B
O�B
N�B
N�B
O�B
Q�B
P�B
P�B
O�B
P�B
R�B
Q�B
P�B
P�B
S�B
VB
VB
VB
VB
T�B
VB
VB
VB
W
B
XB
XB
YB
XB
XB
XB
YB
YB
XB
W
B
XB
YB
ZB
ZB
]/B
\)B
\)B
]/B
\)B
[#B
ZB
[#B
\)B
]/B
^5B
]/B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
_;B
`BB
`BB
_;B
_;B
^5B
_;B
^5B
]/B
^5B
`BB
`BB
`BB
_;B
aHB
`BB
`BB
`BB
bNB
bNB
bNB
bNB
aHB
aHB
`BB
bNB
cTB
bNB
bNB
cTB
bNB
aHB
bNB
bNB
aHB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
e`B
ffB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
gmB
gmB
ffB
ffB
ffB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
gmB
jB
iyB
k�B
l�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
o�B
n�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
m�B
n�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
q�B
s�B
s�B
s�B
r�B
r�B
s�B
r�B
p�B
q�B
u�B
v�B
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
u�B
v�B
v�B
v�B
t�B
u�B
u�B
v�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
y�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
}�B
}�B
}�B
� B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
�B
�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�7B
�1B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�DB
�DB
�JB
�DB
�JB
�JB
�DB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�hB
�hB
�oB
�oB
�oB
�hB
�hB
�hB
�hB
�hB
�uB
�uB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	�B
��BYB�{B�{Bw�BbNBɺB�?B�B�'B�wB�dB�B�dB�/B�/B�B�B��BBoBhB  B�yB�B��BƨB�!B��B�B]/BG�B�B
�#B
�qB
�B
��B
�1B
t�B
S�B
=qB
0!B
1B
%B

=B	��B	�B	�fB	��B	��B	�9B	�!B	��B	v�B	-B	B�B�sB�B�5B�#B��BɺB�5B�;B�
B��B��B�#B�sB��B�B�B�yB�B	B	\B	uB	�B	�B	&�B	+B	.B	/B	I�B	K�B	B�B	cTB	y�B	�B	�B	y�B	k�B	�%B	�VB	�{B	�%B	��B	�'B	�jB	B	�wB	��B	ÖB	��B	�B	�B	�B	�
B	��B	�TB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
	7B
PB
hB
uB
�B
�B
�B
�B
hB
{B
JB
hB
{B
{B
{B
oB
VB
PB
JB
PB
�B
�B
�B
�B
{B
uB
uB
uB
�B
hB
\B
�B
�B
bB
bB
oB
oB
uB
{B
{B
{B
uB
oB
hB
bB
hB
hB
oB
oB
hB
\B
bB
\B
VB
VB
DB
DB
DB

=B
JB
PB
PB
JB
PB
PB
DB
+B
%B
+B
B
B
B
B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B	��B
B
  B
B
  B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
  B	��B	��B
  B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
1B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
!�B
!�B
�B
�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
!�B
 �B
 �B
!�B
#�B
#�B
#�B
"�B
!�B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
!�B
"�B
"�B
!�B
"�B
%�B
%�B
#�B
 �B
%�B
&�B
%�B
&�B
&�B
%�B
%�B
#�B
$�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
)�B
(�B
)�B
+B
,B
,B
+B
+B
+B
-B
-B
,B
+B
+B
+B
.B
/B
/B
/B
-B
,B
.B
-B
+B
,B
-B
.B
.B
/B
1'B
1'B
1'B
0!B
.B
/B
1'B
1'B
/B
0!B
2-B
1'B
0!B
0!B
1'B
2-B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
49B
5?B
5?B
5?B
33B
7LB
9XB
9XB
9XB
9XB
8RB
9XB
8RB
8RB
8RB
7LB
8RB
7LB
49B
9XB
8RB
9XB
:^B
<jB
<jB
<jB
;dB
;dB
<jB
=qB
<jB
>wB
>wB
=qB
>wB
>wB
@�B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
?}B
>wB
A�B
A�B
B�B
B�B
A�B
A�B
B�B
B�B
B�B
A�B
A�B
C�B
B�B
C�B
E�B
D�B
C�B
B�B
B�B
A�B
D�B
E�B
E�B
C�B
D�B
F�B
F�B
E�B
E�B
F�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
L�B
K�B
N�B
N�B
O�B
N�B
N�B
O�B
Q�B
P�B
P�B
O�B
P�B
R�B
Q�B
P�B
P�B
S�B
VB
VB
VB
VB
T�B
VB
VB
VB
W
B
XB
XB
YB
XB
XB
XB
YB
YB
XB
W
B
XB
YB
ZB
ZB
]/B
\)B
\)B
]/B
\)B
[#B
ZB
[#B
\)B
]/B
^5B
]/B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
_;B
`BB
`BB
_;B
_;B
^5B
_;B
^5B
]/B
^5B
`BB
`BB
`BB
_;B
aHB
`BB
`BB
`BB
bNB
bNB
bNB
bNB
aHB
aHB
`BB
bNB
cTB
bNB
bNB
cTB
bNB
aHB
bNB
bNB
aHB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
e`B
ffB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
gmB
gmB
ffB
ffB
ffB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
gmB
jB
iyB
k�B
l�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
o�B
n�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
m�B
n�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
q�B
s�B
s�B
s�B
r�B
r�B
s�B
r�B
p�B
q�B
u�B
v�B
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
u�B
v�B
v�B
v�B
t�B
u�B
u�B
v�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
y�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
}�B
}�B
}�B
� B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
�B
�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�7B
�1B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�DB
�DB
�JB
�DB
�JB
�JB
�DB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�hB
�hB
�oB
�oB
�oB
�hB
�hB
�hB
�hB
�hB
�uB
�uB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230111080134                              AO  ARCAADJP                                                                    20230111080134    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230111080134  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230111080134  QCF$                G�O�G�O�G�O�0               