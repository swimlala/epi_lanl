CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-04-07T00:35:29Z creation;2017-04-07T00:35:32Z conversion to V3.1;2019-12-19T08:10:53Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170407003529  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA  I2_0577_106                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����p� 1   @���s�� @3�u�!�S�d�8�4֡1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D�  D�@ Dր D�� D�3D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�
D4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDG�DG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz��Dz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�Dҁ�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�D���D���D�>�D�~�Dվ�D���D�>�D�~�D־�D��D�A�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�;�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�v�A�v�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�x�A�x�A�v�A�r�A�jA�A� �A�VA��/A͋DA�l�A�M�A̩�A�A�A�(�A�{A���A�A�A�A��
Aɰ!A���A��A�|�AǋDA�-A�%AƼjA�t�A��
A��A��A��A�bA¸RAuA��A�A�~�A�oA��A��TA���A���A�M�A�7LA�$�A���A�I�A���A��;A��-A��\A�jA�(�A���A�9XA�1A���A�p�A�r�A���A��jA���A�I�A��A���A���A�-A�K�A���A�+A���A�A�l�A�bNA�l�A�=qA� �A���A�XA��;A���A�7LA�E�A���A��-A��A�%A��A�v�A�x�A���A�n�A��DA�9XA�  A�JA���A���A�r�A��mA��uA���A�A|ĜA{�-AxffAuAq�#Am�mAl�Ak33Ah5?Ae�Ad1'Ac�Ab�jAb�Aa��A`{A^�`A]��AZ1AXr�AUdZATAS/AR��ARAQ�7AQ�AM�#AL��AKhsAJ��AG/AD=qACdZAB��A@v�A>��A=��A=%A;A9�;A8�`A81A733A5�^A3"�A2  A0v�A-��A-x�A,�A,~�A+��A)�;A(Q�A'`BA&ȴA&bNA%\)A$�A#x�A"�!A ��A��Ap�A�AC�AjA|�A�A��AhsA��AVAJA��A&�A=qAdZA��A��A1'A�hAjA��A�mA
jA
1A
JA	��AĜAJAp�A|�A��AG�A�A1'AA�AVA�A��A��A �A �A 5?A J@�C�@��@�`B@���@�1@�S�@�$�@�p�@�Z@�\)@���@��u@�~�@���@��;@�S�@���@�~�@�@�Q�@�
=@�7@�
=@�J@�O�@�Ĝ@�j@��@�t�@��y@��@���@߮@ޏ\@܃@�v�@�`B@��@؃@��m@ׅ@ְ!@��T@�@Չ7@��/@��
@��@�7L@�G�@Ь@��@�p�@�V@��`@�I�@�|�@�ȴ@�O�@�ƨ@�+@�=q@�p�@�V@ģ�@�z�@�(�@Å@+@��7@��@�&�@���@�ƨ@���@�^5@�@���@��-@��7@��@���@�A�@�  @���@�l�@���@�{@��H@�l�@�5?@�Ĝ@�1'@���@��!@��@�x�@���@��@�`B@�hs@���@�?}@��@��m@�S�@�+@�
=@��y@�v�@��7@���@��F@���@���@�M�@��@�\)@���@��h@�J@��@�X@��`@��;@��y@���@�7L@�%@�z�@��u@�Q�@�9X@��j@�/@�G�@�G�@��j@�I�@�b@�l�@�$�@��^@�V@�ff@�n�@��+@���@�bN@�Q�@�1@�1@�b@���@�1@��@��m@��F@���@�+@���@���@�E�@�M�@�n�@�v�@��\@��\@�ff@��-@�?}@��`@��@��`@��/@��j@��9@��@�b@��F@��@�+@��+@��@��T@�@�hs@�Ĝ@�Z@�(�@� �@� �@�  @���@�|�@��@���@�^5@�=q@��@���@�`B@��@���@��9@�  @��w@��@�+@���@�V@�^5@�$�@���@�G�@�x�@��h@�p�@�G�@�/@��@��`@���@�A�@��;@��w@���@�|�@�K�@�;d@�@��R@��\@�^5@���@���@�?}@�Ĝ@�j@�b@��m@���@�;d@��@�ȴ@��!@�^5@�@��@���@�hs@�?}@��/@�r�@�(�@��m@��@�\)@��@�@��H@�ff@�-@��@��@��h@�?}@�V@��@���@��j@�1'@�1@��@��F@�33@���@��!@���@�~�@�^5@�M�@�M�@�E�@�E�@�5?@�$�@��^@�%@��D@�Z@�Q�@�9X@��w@�t�@�;d@�@��y@���@���@�V@�{@��7@���@��@�(�@� �@�  @�w@
=@~��@~v�@}�-@}V@|�/@|�@|��@|j@{�
@{�F@{C�@z�\@z-@x��@x�9@xA�@x �@x �@x  @w��@w�w@w|�@v��@v�y@v�y@v�+@u��@u�h@u�@tI�@s��@sƨ@sS�@so@r��@r-@q&�@p�9@p�u@pA�@o�;@o\)@n��@nV@n$�@m@m?}@m�@l��@lI�@k��@k"�@k@j�@j�!@j��@j~�@j=q@i�#@ihs@i&�@h�9@hQ�@h1'@g�@g��@g�w@g�@g��@gl�@gK�@g;d@f�y@fȴ@f�+@fE�@e�T@e�@ep�@e`B@d��@dz�@dj@dI�@d(�@d�@d1@c��@c�
@c��@ct�@b��@b=q@a��@a�@a��@a7L@`Ĝ@`bN@` �@_��@_�P@_;d@^�y@^��@^��@^v�@^$�@]�@]�-@]`B@\�@\�@\�@[��@[��@[�m@[�F@[C�@Z�@Z��@Z^5@Y��@Y�@Y�@Y�#@Yhs@X��@X�@X �@W��@W��@Wl�@Wl�@W;d@V��@Vȴ@V��@V�+@V5?@V@U�@U`B@T��@T�D@Tj@T9X@T�@T�@T1@S�
@S��@St�@S33@R�@R��@R^5@R=q@R=q@Q��@Qx�@Q%@P�u@P�@Pr�@PbN@PbN@PQ�@Pb@O|�@O+@N��@NV@N{@M�T@M�h@L��@L�/@L�j@Lj@L�@L(�@K��@KS�@KC�@J�@J�H@J��@J��@J~�@I��@IX@I%@H��@Hb@G�@G|�@G\)@G\)@GK�@GK�@G+@F��@Fv�@F{@E@Ep�@D�@DZ@D�@CC�@B��@B�!@B^5@A�#@A��@Ax�@A%@@�u@@Q�@@ �@?�@?�w@?
=@>ff@>5?@=�@=�-@=?}@=V@<��@<��@<z�@<9X@;�
@;dZ@;o@:�H@:�\@:n�@:^5@9��@8��@8�@8  @7�P@7�@6�y@6�R@6�+@6@5�h@5�@5p�@5?}@5�@4�@4�@4�@4�D@4Z@41@3�
@3t�@2�@2~�@2M�@2=q@2J@1�^@1X@1&�@0Ĝ@0r�@0Q�@01'@/�@/��@/+@/
=@.��@.�y@.��@.�+@.ff@.{@-�T@-�-@-p�@,�/@,��@,��@,9X@,(�@,(�@,�@+��@+ƨ@+t�@+o@*�@*��@*�!@*��@*n�@)��@)�^@)��@)��@)�7@)hs@)&�@(��@(�9@(�u@(�@(bN@(A�@'�@'\)@'+@&�@&ȴ@&�R@&v�@%�T@%�h@%�@%`B@%O�@%/@%�@%�@%V@$��@$�/@$�j@$z�@$I�@#�
@#S�@"��@"�!@"�!@"��@"^5@!��@!��@!X@!G�@!7L@!%@ �9@ 1'@ b@ b@ b@ b@�@�@\)@�@
=@��@�y@ȴ@�+@E�@��@�-@��@p�@V@��@�@�@�@��@��@��@��@��@z�@Z@9X@1@�m@ƨ@"�@��@�@��@��@�@�@�@��@�7@&�@�`@Ĝ@�u@bN@b@  @��@��@l�@\)@K�@+@��@�@E�@$�@$�@{@��@��@�@/@��@�/@�@j@I�@(�@��@ƨ@�F@t�@33@�@��@^5@M�@J@��@��@7L@��@��@�9@��@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�v�A�v�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�x�A�x�A�v�A�r�A�jA�A� �A�VA��/A͋DA�l�A�M�A̩�A�A�A�(�A�{A���A�A�A�A��
Aɰ!A���A��A�|�AǋDA�-A�%AƼjA�t�A��
A��A��A��A�bA¸RAuA��A�A�~�A�oA��A��TA���A���A�M�A�7LA�$�A���A�I�A���A��;A��-A��\A�jA�(�A���A�9XA�1A���A�p�A�r�A���A��jA���A�I�A��A���A���A�-A�K�A���A�+A���A�A�l�A�bNA�l�A�=qA� �A���A�XA��;A���A�7LA�E�A���A��-A��A�%A��A�v�A�x�A���A�n�A��DA�9XA�  A�JA���A���A�r�A��mA��uA���A�A|ĜA{�-AxffAuAq�#Am�mAl�Ak33Ah5?Ae�Ad1'Ac�Ab�jAb�Aa��A`{A^�`A]��AZ1AXr�AUdZATAS/AR��ARAQ�7AQ�AM�#AL��AKhsAJ��AG/AD=qACdZAB��A@v�A>��A=��A=%A;A9�;A8�`A81A733A5�^A3"�A2  A0v�A-��A-x�A,�A,~�A+��A)�;A(Q�A'`BA&ȴA&bNA%\)A$�A#x�A"�!A ��A��Ap�A�AC�AjA|�A�A��AhsA��AVAJA��A&�A=qAdZA��A��A1'A�hAjA��A�mA
jA
1A
JA	��AĜAJAp�A|�A��AG�A�A1'AA�AVA�A��A��A �A �A 5?A J@�C�@��@�`B@���@�1@�S�@�$�@�p�@�Z@�\)@���@��u@�~�@���@��;@�S�@���@�~�@�@�Q�@�
=@�7@�
=@�J@�O�@�Ĝ@�j@��@�t�@��y@��@���@߮@ޏ\@܃@�v�@�`B@��@؃@��m@ׅ@ְ!@��T@�@Չ7@��/@��
@��@�7L@�G�@Ь@��@�p�@�V@��`@�I�@�|�@�ȴ@�O�@�ƨ@�+@�=q@�p�@�V@ģ�@�z�@�(�@Å@+@��7@��@�&�@���@�ƨ@���@�^5@�@���@��-@��7@��@���@�A�@�  @���@�l�@���@�{@��H@�l�@�5?@�Ĝ@�1'@���@��!@��@�x�@���@��@�`B@�hs@���@�?}@��@��m@�S�@�+@�
=@��y@�v�@��7@���@��F@���@���@�M�@��@�\)@���@��h@�J@��@�X@��`@��;@��y@���@�7L@�%@�z�@��u@�Q�@�9X@��j@�/@�G�@�G�@��j@�I�@�b@�l�@�$�@��^@�V@�ff@�n�@��+@���@�bN@�Q�@�1@�1@�b@���@�1@��@��m@��F@���@�+@���@���@�E�@�M�@�n�@�v�@��\@��\@�ff@��-@�?}@��`@��@��`@��/@��j@��9@��@�b@��F@��@�+@��+@��@��T@�@�hs@�Ĝ@�Z@�(�@� �@� �@�  @���@�|�@��@���@�^5@�=q@��@���@�`B@��@���@��9@�  @��w@��@�+@���@�V@�^5@�$�@���@�G�@�x�@��h@�p�@�G�@�/@��@��`@���@�A�@��;@��w@���@�|�@�K�@�;d@�@��R@��\@�^5@���@���@�?}@�Ĝ@�j@�b@��m@���@�;d@��@�ȴ@��!@�^5@�@��@���@�hs@�?}@��/@�r�@�(�@��m@��@�\)@��@�@��H@�ff@�-@��@��@��h@�?}@�V@��@���@��j@�1'@�1@��@��F@�33@���@��!@���@�~�@�^5@�M�@�M�@�E�@�E�@�5?@�$�@��^@�%@��D@�Z@�Q�@�9X@��w@�t�@�;d@�@��y@���@���@�V@�{@��7@���@��@�(�@� �@�  @�w@
=@~��@~v�@}�-@}V@|�/@|�@|��@|j@{�
@{�F@{C�@z�\@z-@x��@x�9@xA�@x �@x �@x  @w��@w�w@w|�@v��@v�y@v�y@v�+@u��@u�h@u�@tI�@s��@sƨ@sS�@so@r��@r-@q&�@p�9@p�u@pA�@o�;@o\)@n��@nV@n$�@m@m?}@m�@l��@lI�@k��@k"�@k@j�@j�!@j��@j~�@j=q@i�#@ihs@i&�@h�9@hQ�@h1'@g�@g��@g�w@g�@g��@gl�@gK�@g;d@f�y@fȴ@f�+@fE�@e�T@e�@ep�@e`B@d��@dz�@dj@dI�@d(�@d�@d1@c��@c�
@c��@ct�@b��@b=q@a��@a�@a��@a7L@`Ĝ@`bN@` �@_��@_�P@_;d@^�y@^��@^��@^v�@^$�@]�@]�-@]`B@\�@\�@\�@[��@[��@[�m@[�F@[C�@Z�@Z��@Z^5@Y��@Y�@Y�@Y�#@Yhs@X��@X�@X �@W��@W��@Wl�@Wl�@W;d@V��@Vȴ@V��@V�+@V5?@V@U�@U`B@T��@T�D@Tj@T9X@T�@T�@T1@S�
@S��@St�@S33@R�@R��@R^5@R=q@R=q@Q��@Qx�@Q%@P�u@P�@Pr�@PbN@PbN@PQ�@Pb@O|�@O+@N��@NV@N{@M�T@M�h@L��@L�/@L�j@Lj@L�@L(�@K��@KS�@KC�@J�@J�H@J��@J��@J~�@I��@IX@I%@H��@Hb@G�@G|�@G\)@G\)@GK�@GK�@G+@F��@Fv�@F{@E@Ep�@D�@DZ@D�@CC�@B��@B�!@B^5@A�#@A��@Ax�@A%@@�u@@Q�@@ �@?�@?�w@?
=@>ff@>5?@=�@=�-@=?}@=V@<��@<��@<z�@<9X@;�
@;dZ@;o@:�H@:�\@:n�@:^5@9��@8��@8�@8  @7�P@7�@6�y@6�R@6�+@6@5�h@5�@5p�@5?}@5�@4�@4�@4�@4�D@4Z@41@3�
@3t�@2�@2~�@2M�@2=q@2J@1�^@1X@1&�@0Ĝ@0r�@0Q�@01'@/�@/��@/+@/
=@.��@.�y@.��@.�+@.ff@.{@-�T@-�-@-p�@,�/@,��@,��@,9X@,(�@,(�@,�@+��@+ƨ@+t�@+o@*�@*��@*�!@*��@*n�@)��@)�^@)��@)��@)�7@)hs@)&�@(��@(�9@(�u@(�@(bN@(A�@'�@'\)@'+@&�@&ȴ@&�R@&v�@%�T@%�h@%�@%`B@%O�@%/@%�@%�@%V@$��@$�/@$�j@$z�@$I�@#�
@#S�@"��@"�!@"�!@"��@"^5@!��@!��@!X@!G�@!7L@!%@ �9@ 1'@ b@ b@ b@ b@�@�@\)@�@
=@��@�y@ȴ@�+@E�@��@�-@��@p�@V@��@�@�@�@��@��@��@��@��@z�@Z@9X@1@�m@ƨ@"�@��@�@��@��@�@�@�@��@�7@&�@�`@Ĝ@�u@bN@b@  @��@��@l�@\)@K�@+@��@�@E�@$�@$�@{@��@��@�@/@��@�/@�@j@I�@(�@��@ƨ@�F@t�@33@�@��@^5@M�@J@��@��@7L@��@��@�9@��@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�!B
�B
�B
�B
�!B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�3B
��BBT�Bk�B|�B�B�+B��B��B�B�-B��B�)B�`B�B��B�B6FBO�B^5BbNBdZBhsBjBq�Bv�By�B{�B�B� B~�B�B�B�1B�\B�VB�VB�VB�\B�bB�\B�\B�hB�uB��B��B��B��B��B��B��B�B�!B�B�B�B��B��B�DB{�Bl�BVBI�B<jB1'B0!B.B,B!�BPB��B�B�}B�3B�-B�-B�9B�'B�-B��B��B��B�oB}�Br�Bp�BdZB\)BL�B<jB+B�BB
�sB
�)B
�RB
�B
��B
�+B
o�B
O�B
B�B
(�B
1B	��B	�B	��B	��B	�jB	�B	�B	�B	�B	�B	��B	��B	�hB	�=B	r�B	ffB	VB	I�B	B�B	>wB	;dB	9XB	7LB	2-B	.B	+B	%�B	�B	DB	%B	B��B�B�B�B�ZB�5B�B�
B��B��BŢB��B��B�^B�RB�LB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B�oB�PB�=B�7B�%B�B�B�B�B�B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�+B|�B�B�+B�%B�PB�VB�hB��B��B��B��B��B��B��B�B�B�!B�!B�?B�XB�LB�9B�-B�3B�'B�9B�LB�RB�RB�XB�^B�wBÖBƨBǮBɺBȴBɺB��B��B��B��B��B��B�B�B�B�B�;B�HB�NB�TB�ZB�`B�yB�B�B�B��B��B��B��B��B	B	B	1B	
=B	
=B	DB	\B	bB	PB	
=B	DB	
=B		7B	
=B	VB	\B	{B	�B	�B	�B	�B	�B	 �B	!�B	�B	"�B	%�B	&�B	'�B	(�B	,B	.B	0!B	1'B	2-B	2-B	5?B	;dB	D�B	G�B	H�B	A�B	A�B	?}B	A�B	C�B	H�B	N�B	Q�B	VB	\)B	]/B	gmB	k�B	k�B	l�B	l�B	k�B	m�B	p�B	q�B	q�B	t�B	u�B	t�B	s�B	w�B	y�B	|�B	x�B	|�B	~�B	}�B	}�B	}�B	�B	� B	~�B	� B	� B	�B	�B	�B	�+B	�DB	�JB	�bB	�oB	�oB	��B	��B	�oB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�FB	�qB	��B	��B	B	��B	��B	��B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�BB	�BB	�NB	�NB	�NB	�NB	�NB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB

=B

=B

=B

=B
JB
DB

=B

=B
DB
DB
JB
JB
DB

=B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
VB
\B
\B
bB
oB
oB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
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
,B
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
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
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
;dB
;dB
<jB
<jB
<jB
<jB
<jB
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
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
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
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
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
P�B
P�B
Q�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
[#B
[#B
ZB
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
]/B
^5B
^5B
^5B
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
`BB
`BB
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
bNB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
ffB
gmB
hsB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�!B
�B
�B
�B
�!B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�;B
�AB
��B
��B
��BBT�BlB}<B��B��B�IB�0B�cB��B��BܬB�LB��B iBB7fBQ�B^�Bb�BeBi_BlBsMBx�Bz�B}qB��B��B� B��B��B�B��B�pB��B��B��B��B��B�.B�TB�B��B��B��B�B�pB��B�$B��B�vB�GB��B��B�QB�~B��B�Bq�BYBN�B>BB2�B1�B0!B/�B%�BB�0B�]B��B�TB��B��B�LB��B��B�B��B��B�mB�4Bt�Br�Bf�B_!BO\B?�B.cB�BB
�B
�B
�xB
�5B
��B
�B
r�B
RTB
F�B
-B
0B	��B	�)B	ԕB	�B	�.B	��B	�IB	��B	��B	�IB	��B	�qB	��B	�"B	u?B	i�B	W�B	J�B	CaB	?HB	<PB	:�B	:�B	3�B	0!B	,�B	)�B	�B	�B	�B	�B�B�B�B��B��BߊB�kB�yB�[BοB�zB��B�B�B�$B�8B��B��B��B�]B��B��B�RB�B�hB�VB�5B�B�eB�B��B�xB�rB�B��B��B��B��B��B��B��B�B�NB��B�B��B��B�`B�XB�8B�bB�KB��B�kB��B��B��B��B�B~B��B��B�%B�6B�VB��B��B��B�tB�TB�-B�vB��B�6B��B��B��B�B��B�8B�%B�hB�TB��B�?B�B��B��B��B�0B�cBĜB��B�7B�rB�7B�=B�B�BB�bB�oB��B��B��B�
B׍B�qB��B�B��B��B��B��B��B��B�B�AB��B��B��B�$B��B	'B	�B	�B	
�B	
�B	�B	B	hB	<B	
�B	�B	
�B		�B	
�B	�B	�B	B	SB	B	�B	�B	5B	!�B	"�B	 'B	# B	&B	'B	(>B	)_B	,qB	.cB	0UB	1vB	2aB	2�B	5�B	;B	D�B	H�B	I�B	B'B	B[B	?�B	B'B	DB	IB	N�B	Q�B	VB	\xB	]IB	g�B	l"B	k�B	l�B	l�B	k�B	n/B	q[B	r�B	q�B	uB	vFB	t�B	s�B	w�B	zxB	}VB	x�B	}B	�B	~�B	~�B	~�B	��B	�iB	HB	�OB	� B	�AB	�B	��B	�B	�DB	�~B	��B	��B	��B	�9B	�?B	��B	�NB	��B	��B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	�
B	�0B	�"B	�6B	�kB	�]B	�/B	�[B	�-B	�FB	��B	��B	��B	��B	�B	��B	��B	ŢB	��B	�B	��B	� B	� B	�:B	�@B	�,B	�SB	ڠB	�xB	�jB	��B	�B	�B	�B	�hB	�hB	�hB	�tB	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�2B	�B	��B	��B	�"B	�"B	�B	�B	�(B	�.B
 OB
AB
-B
3B
MB
gB
9B
SB
YB
_B
fB
�B
	�B
	�B
	�B

�B

rB

rB
xB

�B

�B

XB

rB
�B
�B

rB

=B
�B
xB
�B
�B
xB

rB
	lB
	lB
	lB

XB

rB
�B
xB
^B
�B
�B
�B
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
B
 B
 'B
 �B
 �B
!�B
!�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"B
!�B
#B
#B
# B
#�B
$B
$&B
%B
%B
%,B
&LB
'B
'B
'B
'8B
'B
($B
($B
(
B
($B
)*B
)*B
)B
)DB
*0B
*0B
+B
+B
+B
+B
+6B
+B
+QB
+6B
,"B
,=B
,"B
-)B
-CB
-)B
-)B
-B
-)B
-)B
-)B
-)B
-)B
.IB
./B
.IB
.IB
./B
/5B
/OB
/OB
/5B
0;B
0UB
0;B
0!B
0!B
0;B
0;B
0;B
0UB
0oB
1[B
1AB
1[B
1[B
2aB
2aB
2aB
2GB
3MB
3MB
3hB
3MB
4nB
4TB
4TB
4TB
4TB
4TB
4�B
5tB
5ZB
5tB
5ZB
5?B
5ZB
5ZB
5tB
6`B
6`B
6`B
6`B
7LB
7LB
7fB
7�B
7�B
8�B
8lB
8lB
9rB
9rB
9XB
9�B
9rB
:xB
:xB
:xB
:xB
:xB
:xB
:�B
;�B
;B
;B
<�B
<�B
<jB
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?}B
?}B
?}B
?}B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
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
H�B
IB
H�B
I�B
J	B
I�B
J	B
J	B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
MB
L�B
L�B
M�B
N"B
OB
OB
N�B
N�B
OB
PB
P.B
Q B
Q B
Q B
QB
Q B
QB
R:B
T,B
T,B
UMB
UMB
UB
UB
V9B
VSB
V9B
VB
VB
V9B
W$B
W?B
W$B
W$B
W$B
W?B
W?B
W$B
W?B
WYB
X+B
X+B
X+B
X+B
X+B
Y1B
Y1B
YeB
ZQB
[=B
[=B
ZQB
[=B
[=B
[=B
[#B
[WB
\CB
\CB
\CB
\CB
\CB
\CB
]IB
]~B
]IB
]dB
^OB
^5B
^5B
^OB
^jB
^jB
^OB
_VB
_pB
_VB
_VB
_pB
_VB
_pB
`vB
`BB
`BB
`\B
`\B
`\B
abB
abB
abB
abB
a|B
abB
a�B
bhB
b�B
b�B
cTB
c�B
b�B
c�B
cnB
cTB
c�B
cTB
cnB
cTB
cTB
cTB
cTB
cnB
c�B
cnB
cnB
c�B
d�B
d�B
e`B
e`B
ezB
ezB
e�B
f�B
f�B
ffB
f�B
f�B
f�B
g�B
hsB
hsB
gmB
gmB
g�B
g�B
h�B
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
jB
jB
jB
k�B
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201704110035132017041100351320170411003513201806221311422018062213114220180622131142201804050712532018040507125320180405071253  JA  ARFMdecpA19c                                                                20170407093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170407003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170407003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170407003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170407003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170407003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170407003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170407003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170407003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170407003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20170407010600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170407153608  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170410153513  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170410153513  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221253  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041142  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                