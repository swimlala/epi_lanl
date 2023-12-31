CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-28T00:35:32Z creation;2018-02-28T00:35:36Z conversion to V3.1;2019-12-19T07:44:06Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180228003532  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_215                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�O���j 1   @�O��I�@3q���'R�d>u%F1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A`��A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C8\C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU�)CW��CY��C[��C^\C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm��Dm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�A�DÁ�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��TA��`A�"�A�9XA�E�A�I�A�M�A�O�A�M�A�K�A�O�A�O�A�=qA���A��AɼjAɥ�A�~�A�=qA���A��A���AȸRAȩ�AȑhAȁA�z�A�t�A�Q�A���A�p�A�C�A�33A�$�A�A���A���A�VA�1'A��AōPA�/A�bA���A��A��mA���A�p�A�  A�ȴA�ffA��A��PA���A��A���A��!A��A�S�A�A��uA�5?A��A�Q�A���A��mA���A�ƨA�+A�oA��A�=qA�7LA�dZA���A�1'A�/A�~�A��RA��yA�{A���A�G�A��A�ƨA�Q�A�9XA��;A��A��A�VA��A�VA���A�z�A�`BA��^A��A�^5A���A�bA�VA�r�A��A���A��A��!A��-A�VA�=qA�A�{A�^5A�p�A�ffA��A�33A~��A{K�Azv�AvĜArȴAqƨAp �Al9XAhz�Ae�wAbE�A`�A\1'AZ�`AX�`AVbAU�PAT^5AQ�AMhsAI�AH~�AH$�AFbAD(�ABr�AAx�A@��A?
=A=�mA=�A=?}A<z�A:��A8�9A6I�A5/A4�!A3�TA3O�A4  A4z�A4JA3O�A2{A1A1A0�A.��A.5?A-�
A,JA*ffA)&�A(9XA($�A&�A#�mA"ȴA!��Al�A$�A�FA��AffA�A��A=qAG�A�A�A^5A��AdZA1A��A��AA�A�A
�`A
9XA	�^A	t�A�AffA`BA�HA�DA^5Ax�AȴAz�AG�AM�A&�A 5?@�t�@�?}@��P@�S�@��H@�$�@��@�  @�-@�?}@��@�hs@�o@���@�w@畁@睲@畁@�l�@�K�@�C�@���@柾@�+@�{@�p�@��@�A�@���@�\)@�S�@ް!@ݡ�@���@�Q�@܃@�?}@ܓu@�&�@� �@ّh@�@ٙ�@�(�@�(�@�A�@ם�@�b@�Q�@�b@ӶF@�bN@�r�@� �@Ӿw@��@���@�G�@�j@���@�O�@�
=@�V@�+@�l�@��@���@��D@��@��@� �@���@�~�@��P@��@� �@��`@���@�9X@��@��h@�/@�?}@��-@���@��@��@�33@��@�^5@�~�@���@�t�@��;@� �@�  @��@�dZ@�;d@��H@�n�@�V@��@��@�@�X@�Ĝ@�Ĝ@�(�@�@�S�@��w@��w@���@�
=@���@�ff@���@�@�9X@��@��H@��R@�M�@�@���@��@��^@��#@��-@��9@��@���@�1@��H@���@��7@�&�@�&�@�&�@�^5@�J@��@�O�@��m@��F@��@�1@�K�@�n�@�G�@��@���@��@��;@�C�@�ȴ@�E�@���@�`B@���@��u@��u@���@��@��@��u@�j@�A�@�1'@�1'@�A�@�A�@�1@��@��F@�dZ@��@��R@��!@��!@���@��@���@���@�p�@�/@��@��/@�z�@� �@� �@�1@��@��m@��w@���@�t�@�C�@�33@���@���@���@���@�ȴ@�ȴ@�ȴ@�^5@���@�`B@�`B@�X@�O�@�/@��@���@��u@���@���@�Q�@�1'@�(�@�b@�b@�ƨ@��P@�\)@�;d@���@��!@��+@�M�@��@��T@��^@��@�/@��@��D@��F@�t�@�\)@�;d@�o@��y@��@��H@�M�@�V@�=q@���@�`B@�O�@��/@�Z@�b@��
@��F@���@��@�S�@�@���@���@��y@�o@�
=@��@�$�@�G�@�V@��/@�j@�9X@��@���@�\)@�dZ@�;d@���@���@���@���@��+@�~�@�~�@�v�@�5?@��@�O�@�&�@�V@�/@�p�@�G�@���@��@�z�@�j@�1'@��m@��@���@��@��w@���@�K�@���@���@�ff@�M�@�J@��-@�?}@��@���@���@�A�@�@�P@\)@+@~��@~�y@~�R@~E�@~{@~@}�h@|�@|j@|1@|1@{��@{��@{��@{��@{�m@{�@z�@z�!@z~�@z^5@y��@yX@y%@x1'@w��@w�P@wK�@v�y@v�R@v��@v5?@u@t�/@s�@qx�@p��@pA�@o�w@o+@nff@m�-@m�@mp�@m?}@m�@l��@l�@l��@l��@lz�@lZ@l1@k�
@k��@k��@kt�@j��@i��@ix�@h��@h�u@hr�@h1'@h  @g�w@g�P@gl�@g\)@g\)@f�R@f@e�h@e�@ep�@e?}@d�j@d9X@d(�@d(�@d�@c��@c�m@c�
@c��@c33@b-@a��@aX@aG�@`��@`r�@`1'@_�@_|�@_+@_�@_
=@_
=@^�y@^�R@^�+@^ff@]?}@\��@\j@\1@[�F@[t�@[C�@[C�@["�@Z��@Z^5@Z-@YX@X��@X��@X �@W�w@W|�@V��@U�@U�@T9X@T(�@T(�@T�@S�m@St�@SC�@S@R�\@R=q@R�@R�@Q��@Qx�@Qhs@QX@QG�@QG�@P�u@O�@O�P@O�@NE�@N$�@N{@N@M��@Mp�@M?}@MV@L�@L�/@L��@L�@Lz�@L9X@K�F@K��@K��@K�@Kt�@J�H@J-@I��@I��@H��@H1'@G�P@G;d@G
=@G
=@F�y@Fff@F5?@E�-@E`B@E?}@E/@EV@D�j@D��@DZ@C�m@C��@Co@B�!@B��@B��@BJ@A�#@A��@Ax�@AX@@Ĝ@?�@?l�@?\)@?K�@?K�@?;d@?+@?;d@?+@?�@?�@>��@>��@>5?@=��@=�@<��@<I�@;��@;S�@;o@:^5@9�@9��@9��@9X@8��@8�@8  @7�@7\)@6��@6�+@6{@5�T@5�-@5��@5�@5O�@5?}@5V@4�/@4�@4Z@4�@3��@3t�@333@3"�@2�@2��@2^5@2=q@2�@1��@1�#@1��@1G�@1%@01'@/�@/�@/;d@/
=@.�R@.��@.��@.��@.��@.��@.ff@.5?@-�-@,��@,��@,�D@,I�@+�m@+�
@+�
@+ƨ@+�@+S�@+33@+@*��@*~�@)�@)�7@)7L@(��@(�@(Q�@(1'@'��@'�w@'�w@'�w@'�w@'�@'\)@'�@'�@'
=@'
=@&��@&��@&��@&�y@&��@&@%��@%�-@%�@%?}@$�@$j@$I�@$(�@$�@$1@#�
@#�F@#o@"�\@"-@!��@!�^@!��@!X@ �`@ �9@ �u@ A�@�@�P@\)@
=@�R@�+@ff@$�@{@�T@�h@`B@?}@��@�j@�D@j@�m@C�@o@o@@�@��@��@��@n�@�@x�@7L@&�@�@��@Ĝ@��@�@b@|�@�R@�+@�+@�+@v�@ff@ff@ff@ff@ff@ff@V@V@V@E�@$�@$�@{@{@�@�@�@��@p�@�@�D@1@�m@��@S�@@��@�!@�\@~�@n�@^5@=q@-@�@J@�@�#@�#@�#@��@��@��@x�@x�@hs@X@X@7L@�@%@Ĝ@��@��@�@r�@ �@�@|�@l�@l�@l�@\)@+@�y@��@v�@ff@E�@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��TA��`A�"�A�9XA�E�A�I�A�M�A�O�A�M�A�K�A�O�A�O�A�=qA���A��AɼjAɥ�A�~�A�=qA���A��A���AȸRAȩ�AȑhAȁA�z�A�t�A�Q�A���A�p�A�C�A�33A�$�A�A���A���A�VA�1'A��AōPA�/A�bA���A��A��mA���A�p�A�  A�ȴA�ffA��A��PA���A��A���A��!A��A�S�A�A��uA�5?A��A�Q�A���A��mA���A�ƨA�+A�oA��A�=qA�7LA�dZA���A�1'A�/A�~�A��RA��yA�{A���A�G�A��A�ƨA�Q�A�9XA��;A��A��A�VA��A�VA���A�z�A�`BA��^A��A�^5A���A�bA�VA�r�A��A���A��A��!A��-A�VA�=qA�A�{A�^5A�p�A�ffA��A�33A~��A{K�Azv�AvĜArȴAqƨAp �Al9XAhz�Ae�wAbE�A`�A\1'AZ�`AX�`AVbAU�PAT^5AQ�AMhsAI�AH~�AH$�AFbAD(�ABr�AAx�A@��A?
=A=�mA=�A=?}A<z�A:��A8�9A6I�A5/A4�!A3�TA3O�A4  A4z�A4JA3O�A2{A1A1A0�A.��A.5?A-�
A,JA*ffA)&�A(9XA($�A&�A#�mA"ȴA!��Al�A$�A�FA��AffA�A��A=qAG�A�A�G�O�G�O�AdZA1A��A��AA�A�A
�`A
9XA	�^A	t�A�AffA`BA�HA�DA^5Ax�AȴAz�AG�AM�A&�A 5?@�t�@�?}@��P@�S�@��H@�$�@��@�  @�-@�?}@��@�hs@�o@���@�w@畁@睲@畁@�l�@�K�@�C�@���@柾@�+@�{@�p�@��@�A�@���@�\)@�S�@ް!@ݡ�@���@�Q�@܃@�?}@ܓu@�&�@� �@ّh@�@ٙ�@�(�@�(�@�A�@ם�@�b@�Q�@�b@ӶF@�bN@�r�@� �@Ӿw@��@���@�G�@�j@���@�O�@�
=@�V@�+@�l�@��@���@��D@��@��@� �@���@�~�@��P@��@� �@��`@���@�9X@��@��h@�/@�?}@��-@���@��@��@�33@��@�^5@�~�@���@�t�@��;@� �@�  @��@�dZ@�;d@��H@�n�@�V@��@��@�@�X@�Ĝ@�Ĝ@�(�@�@�S�@��w@��w@���@�
=@���@�ff@���@�@�9X@��@��H@��R@�M�@�@���@��@��^@��#@��-@��9@��@���@�1@��H@���@��7@�&�@�&�@�&�@�^5@�J@��@�O�@��m@��F@��@�1@�K�@�n�@�G�@��@���@��@��;@�C�@�ȴ@�E�@���@�`B@���@��u@��u@���@��@��@��u@�j@�A�@�1'@�1'@�A�@�A�@�1@��@��F@�dZ@��@��R@��!@��!@���@��@���@���@�p�@�/@��@��/@�z�@� �@� �@�1@��@��m@��w@���@�t�@�C�@�33@���@���@���@���@�ȴ@�ȴ@�ȴ@�^5@���@�`B@�`B@�X@�O�@�/@��@���@��u@���@���@�Q�@�1'@�(�@�b@�b@�ƨ@��P@�\)@�;d@���@��!@��+@�M�@��@��T@��^@��@�/@��@��D@��F@�t�@�\)@�;d@�o@��y@��@��H@�M�@�V@�=q@���@�`B@�O�@��/@�Z@�b@��
@��F@���@��@�S�@�@���@���@��y@�o@�
=@��@�$�@�G�@�V@��/@�j@�9X@��@���@�\)@�dZ@�;d@���@���@���@���@��+@�~�@�~�@�v�@�5?@��@�O�@�&�@�V@�/@�p�@�G�@���@��@�z�@�j@�1'@��m@��@���@��@��w@���@�K�@���@���@�ff@�M�@�J@��-@�?}@��@���@���@�A�@�@�P@\)@+@~��@~�y@~�R@~E�@~{@~@}�h@|�@|j@|1@|1@{��@{��@{��@{��@{�m@{�@z�@z�!@z~�@z^5@y��@yX@y%@x1'@w��@w�P@wK�@v�y@v�R@v��@v5?@u@t�/@s�@qx�@p��@pA�@o�w@o+@nff@m�-@m�@mp�@m?}@m�@l��@l�@l��@l��@lz�@lZ@l1@k�
@k��@k��@kt�@j��@i��@ix�@h��@h�u@hr�@h1'@h  @g�w@g�P@gl�@g\)@g\)@f�R@f@e�h@e�@ep�@e?}@d�j@d9X@d(�@d(�@d�@c��@c�m@c�
@c��@c33@b-@a��@aX@aG�@`��@`r�@`1'@_�@_|�@_+@_�@_
=@_
=@^�y@^�R@^�+@^ff@]?}@\��@\j@\1@[�F@[t�@[C�@[C�@["�@Z��@Z^5@Z-@YX@X��@X��@X �@W�w@W|�@V��@U�@U�@T9X@T(�@T(�@T�@S�m@St�@SC�@S@R�\@R=q@R�@R�@Q��@Qx�@Qhs@QX@QG�@QG�@P�u@O�@O�P@O�@NE�@N$�@N{@N@M��@Mp�@M?}@MV@L�@L�/@L��@L�@Lz�@L9X@K�F@K��@K��@K�@Kt�@J�H@J-@I��@I��@H��@H1'@G�P@G;d@G
=@G
=@F�y@Fff@F5?@E�-@E`B@E?}@E/@EV@D�j@D��@DZ@C�m@C��@Co@B�!@B��@B��@BJ@A�#@A��@Ax�@AX@@Ĝ@?�@?l�@?\)@?K�@?K�@?;d@?+@?;d@?+@?�@?�@>��@>��@>5?@=��@=�@<��@<I�@;��@;S�@;o@:^5@9�@9��@9��@9X@8��@8�@8  @7�@7\)@6��@6�+@6{@5�T@5�-@5��@5�@5O�@5?}@5V@4�/@4�@4Z@4�@3��@3t�@333@3"�@2�@2��@2^5@2=q@2�@1��@1�#@1��@1G�@1%@01'@/�@/�@/;d@/
=@.�R@.��@.��@.��@.��@.��@.ff@.5?@-�-@,��@,��@,�D@,I�@+�m@+�
@+�
@+ƨ@+�@+S�@+33@+@*��@*~�@)�@)�7@)7L@(��@(�@(Q�@(1'@'��@'�w@'�w@'�w@'�w@'�@'\)@'�@'�@'
=@'
=@&��@&��@&��@&�y@&��@&@%��@%�-@%�@%?}@$�@$j@$I�@$(�@$�@$1@#�
@#�F@#o@"�\@"-@!��@!�^@!��@!X@ �`@ �9@ �u@ A�@�@�P@\)@
=@�R@�+@ff@$�@{@�T@�h@`B@?}@��@�j@�D@j@�m@C�@o@o@@�@��@��@��@n�@�@x�@7L@&�@�@��@Ĝ@��@�@b@|�@�R@�+@�+@�+@v�@ff@ff@ff@ff@ff@ff@V@V@V@E�@$�@$�@{@{@�@�@�@��@p�@�@�D@1@�m@��@S�@@��@�!@�\@~�@n�@^5@=q@-@�@J@�@�#@�#@�#@��@��@��@x�@x�@hs@X@X@7L@�@%@Ĝ@��@��@�@r�@ �@�@|�@l�@l�@l�@\)@+@�y@��@v�@ff@E�@�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�BVB�B�B �B!�B!�B!�B"�B#�B#�B%�B33B8RB:^B;dB@�BI�B\)BjBn�Bp�Bp�Bt�Bv�Bt�Bq�Bo�Br�B�hB��B��B��B�B�B�'B��B��B�B�B+BVBoBoB\B	7B��B��B��B+B+B�`B��BÖBǮB�;B��B��BB��B��BhB1'B6FBL�BA�B+B6FB0!B/B#�B�B�BbB��B�B�5B�B��B�B�
BĜB�wB�!B�LB�wB��B�-B��B��B��B� B��B�bB}�B`BBYB�B�B\B
�`B
��B
�}B
�B
��B
�^B
�XB
�RB
��B
iyB
|�B
m�B
XB
N�B
&�B	�TB	�B
  B	�
B	��B	�?B	��B	m�B	N�B	8RB	'�B	+B	�B	'�B	%�B	B	!�B	�B�B�}B�3BÖBĜB�^B�B�?B�3B�?B��B�!B�jB�XB�B�{B�oB�\B��B�9B�9B�}B�5B�B�B�TB�HB��B	  B��B��B��B	%B�B�B�sB�`B�B��B�B�'B��B�hB��B��B�PBr�Bn�Br�BgmBffBiyBcTB�1B��B� Bu�B<jBYB]/B`BBVB]/BcTBe`B^5B^5BYB`BBaHB`BBXB[#B`BBS�BP�BR�BN�BT�BM�BN�B]/BYBP�BD�B49BI�BN�BE�BI�BI�BN�B]/Bo�Bs�Bt�Bv�By�Bz�By�B� B� B{�Bw�Bs�Bk�B`BBVBn�Bk�Be`BiyBm�Bu�B~�B{�B�DB�+Bz�B�uB��B�uB��B��B��B�VB�B�!B�3BƨBȴBƨBÖB�}B�qB�wB�}B�XB�jB�^BÖB�^B�B��B��B�B�;B�fB�fB�ZB��BĜBŢB��B�HB�5B�B��B�B�NB�sB�B�yB�B�B�B��B��B	
=B	�B	!�B	$�B	#�B	'�B	)�B	-B	/B	-B	/B	0!B	1'B	33B	5?B	0!B	/B	49B	0!B	.B	8RB	@�B	:^B	9XB	5?B	33B	;dB	D�B	7LB	/B	5?B	>wB	@�B	@�B	B�B	F�B	S�B	bNB	dZB	cTB	cTB	cTB	n�B	iyB	gmB	m�B	hsB	n�B	v�B	w�B	�VB	�+B	�7B	�=B	�%B	�bB	��B	��B	�bB	�JB	�\B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�FB	�LB	�XB	�dB	�qB	�qB	�qB	�jB	��B	��B	�wB	��B	ĜB	ǮB	ǮB	ƨB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�
B	�B	�B	�/B	�;B	�BB	�HB	�BB	�/B	�B	�BB	�ZB	�ZB	�`B	�ZB	�ZB	�ZB	�fB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
+B
+B
1B
1B
1B
1B
+B
B
B
B
DB
PB
\B
oB
\B
VB
VB
VB
bB
VB
PB
VB
\B
hB
{B
uB
oB
oB
uB
uB
�B
{B
uB
{B
{B
�B
{B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
$�B
%�B
$�B
$�B
%�B
%�B
%�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
"�B
!�B
$�B
%�B
&�B
(�B
'�B
(�B
(�B
(�B
)�B
)�B
(�B
%�B
%�B
(�B
+B
+B
)�B
)�B
)�B
-B
-B
-B
,B
,B
,B
+B
(�B
&�B
+B
.B
.B
-B
-B
.B
.B
.B
0!B
1'B
1'B
1'B
0!B
0!B
/B
.B
,B
.B
2-B
1'B
2-B
2-B
33B
49B
33B
1'B
1'B
2-B
0!B
2-B
33B
2-B
33B
33B
1'B
2-B
33B
49B
9XB
9XB
9XB
8RB
8RB
8RB
9XB
8RB
9XB
;dB
;dB
:^B
9XB
;dB
<jB
;dB
:^B
8RB
8RB
:^B
:^B
9XB
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
@�B
?}B
?}B
?}B
A�B
A�B
A�B
@�B
>wB
=qB
@�B
@�B
=qB
?}B
@�B
C�B
D�B
E�B
D�B
B�B
D�B
D�B
E�B
F�B
F�B
F�B
E�B
F�B
F�B
D�B
F�B
E�B
F�B
H�B
H�B
F�B
H�B
H�B
H�B
G�B
E�B
D�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
J�B
I�B
I�B
I�B
H�B
K�B
J�B
J�B
L�B
L�B
K�B
M�B
O�B
N�B
N�B
M�B
M�B
M�B
O�B
O�B
O�B
O�B
P�B
R�B
R�B
S�B
S�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
W
B
VB
VB
VB
T�B
T�B
T�B
S�B
W
B
W
B
W
B
YB
XB
[#B
ZB
[#B
ZB
ZB
YB
XB
W
B
VB
ZB
\)B
[#B
ZB
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
ZB
[#B
\)B
]/B
^5B
^5B
_;B
^5B
`BB
aHB
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
_;B
^5B
`BB
aHB
`BB
`BB
_;B
aHB
bNB
cTB
cTB
bNB
bNB
aHB
_;B
`BB
bNB
cTB
cTB
cTB
cTB
bNB
dZB
dZB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
gmB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
iyB
k�B
jB
jB
jB
jB
jB
iyB
gmB
iyB
k�B
l�B
l�B
k�B
k�B
k�B
jB
iyB
iyB
iyB
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
l�B
l�B
m�B
n�B
p�B
o�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
t�B
t�B
s�B
s�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�qB
�B
�eB
�wB"BxB�B �B!�B!�B!�B"�B#�B$&B&fB3hB8�B:�B;�BABJ=B\CBj�Bn�Bp�Bp�Bt�Bv�Bt�Br-BpoBs�B��B��B��B�2B�CB��B�GB��B�}B��B�OBEB�B�B�B�B
rB�cB��B�0B�B	lB��B�$BƎB�#B��B��B��B�B��B��B�B2|B7�BM�BC�B.�B8�B2aB1'B&B�BB B�B�UB��B�B�B�B�	B��B�AB�MB�rB�cB�uB��B��B�$B��B��B��B� B�Bb�B[�B B �BB
�yB
�B
�3B
��B
��B
�PB
�xB
�$B
��B
m�B
~�B
o�B
Z�B
P�B
+�B	�B	�FB
�B	�qB	�0B	��B	�_B	r�B	S@B	<B	,"B	-�B	OB	)�B	(>B	�B	"�B	�B�B��B�fB�BżB�"B��B�2B��B��B�0B�vB�B�B��B�YB��B�B�$B��B�?B� BݘB�MB�qB�B�B�zB	B�HB�cB�B	B�?B�B��B�B�B�?B�GB��B��B�FB�B��B��Bu�Bp�BtTBi�Bh
BkQBezB��G�O�G�O�Bw�BA BZkB^B`�BWsB^BdBe�B_!B_!BZ�B`�Ba�B`�BYeB\)BaBU�BRoBT{BP.BU�BO\BO�B]dBY�BQ�BFYB6�BJ�BO�BGEBK)BK^BPHB]�Bo�Bs�Bt�BwBzB{0BzDB�B�4B|PBxRBt9BlWBa�BWYBn�BlBf2Bi�Bm�Bu�B~�B|jB�^B�1B|�B�[B��B�aB��B��B��B�.B��B�UB��B�tB��B��B�B�4B�BB�B�4B��B��B��BāB��B��B��B�{BچBߤB�B��B��B�mBƨB�B�oB��B�jBخB��B�
B�B�B�B�B�B�B�B�"B��B	
=B	mB	!|B	$�B	#�B	($B	*eB	-CB	/OB	-wB	/�B	0UB	1�B	3hB	5ZB	0�B	/�B	4nB	0�B	.�B	8B	@OB	:�B	9�B	5�B	3�B	;JB	D�B	8RB	0!B	5�B	>�B	@�B	@�B	B�B	F�B	T,B	a�B	d�B	c�B	c�B	c�B	n�B	i�B	h>B	m�B	iDB	n�B	v�B	w�B	��B	��B	��B	��B	�B	��B	��B	��B	� B	�B	�B	��B	��B	��B	��B	�	B	�B	�B	�-B	�2B	�RB	�)B	�!B	�-B	�MB	�MB	�`B	��B	�rB	�B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�2B	�2B	�B	�B	�B	�B	�2B	�$B	�9B	�QB	�IB	�VB	�\B	�bB	�vB	ݘB	ڠB	�\B	�ZB	�B	�zB	�tB	�tB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�!B	��B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B
 �B
 4B	�<B	�rB	�`B	�(B	�(B	�<B	�.B
 4B	�cB
;B
B
MB
{B
+B
EB
KB
KB
1B
KB
_B
SB
SB
�B
^B
PB
BB
TB
�B
�B
�B
�B
}B
�B
�B
pB
vB
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
"�B
"�B
$B
#�B
#�B
!�B
 �B
B
/B
EB
B
B
�B
!B
!-B
!B
$�B
%�B
%B
%B
%�B
%�B
%�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
# B
"B
%B
&B
'B
)B
(
B
)B
)B
)B
*B
*B
)B
&LB
&LB
)B
+B
+6B
*0B
*KB
*KB
-)B
-B
-B
,"B
,"B
,"B
+6B
)DB
'mB
+QB
./B
./B
-CB
-]B
./B
./B
.IB
0;B
1AB
1'B
1AB
0UB
0UB
/5B
.cB
,�B
.IB
2GB
1vB
2aB
2GB
3hB
49B
3MB
1AB
1[B
2GB
0�B
2aB
3MB
2aB
3�B
3hB
1�B
2|B
3�B
4�B
9XB
9XB
9rB
8lB
8�B
8lB
9�B
8�B
9�B
;B
;dB
:xB
9�B
;B
<�B
;B
:xB
8�B
8�B
:�B
:�B
9�B
?�B
?}B
?�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
?�B
?�B
?�B
A�B
A�B
A�B
@�B
>�B
=�B
@�B
@�B
=�B
?�B
@�B
C�B
D�B
E�B
D�B
B�B
D�B
D�B
E�B
F�B
F�B
F�B
E�B
F�B
F�B
D�B
F�B
E�B
F�B
H�B
H�B
F�B
H�B
H�B
H�B
G�B
E�B
D�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
J�B
J	B
I�B
I�B
IB
K�B
J�B
J�B
L�B
L�B
LB
NB
O�B
N�B
N�B
NB
NB
N"B
O�B
O�B
PB
PB
QB
SB
SB
S�B
T,B
SB
S�B
S&B
S&B
SB
SB
SB
S@B
UB
U2B
UB
UB
UB
U2B
W$B
VB
VB
V9B
UB
U2B
UMB
TaB
W$B
W$B
W?B
Y1B
XEB
[#B
ZB
[#B
Z7B
ZB
Y1B
XEB
W?B
VSB
Z7B
\CB
[=B
Z7B
\)B
\)B
\]B
\CB
\CB
\CB
\CB
[=B
[=B
ZQB
[WB
\CB
]IB
^OB
^OB
_VB
^jB
`BB
abB
`\B
`BB
`\B
_VB
`\B
aHB
abB
aHB
aHB
abB
aHB
`\B
_VB
^jB
`\B
abB
`\B
`\B
_pB
abB
bhB
cTB
cTB
bhB
b�B
abB
_�B
`�B
bhB
cnB
cnB
c�B
cnB
b�B
d�B
d�B
c�B
dtB
dtB
ezB
ezB
ezB
f�B
g�B
f�B
g�B
g�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
f�B
f�B
i�B
k�B
j�B
j�B
j�B
j�B
j�B
i�B
g�B
i�B
k�B
l�B
l�B
k�B
k�B
k�B
j�B
i�B
i�B
i�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
l�B
l�B
m�B
n�B
p�B
o�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
t�B
t�B
s�B
s�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803061657232018030616572320180306165723201806221326572018062213265720180622132657201804050730452018040507304520180405073045  JA  ARFMdecpA19c                                                                20180228093522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180228003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180228003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180228003535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180228003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180228003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180228003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180228003536  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180228003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180228003536  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180228003536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180228003536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180228005530                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180228153338  CV  JULD            G�O�G�O�F�}�                JM  ARCAJMQC2.0                                                                 20180306075723  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180306075723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223045  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042657  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                