CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-25T00:36:29Z creation;2018-11-25T00:36:39Z conversion to V3.1;2019-12-19T07:27:18Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181125003629  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              0A   JA  I2_0576_304                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؓ5��1   @ؓ6�>� @9 ���C��d*q���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�|�Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @*=q@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BP=qBW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDcw
Dc�qDd��Dd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�DՁ�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�A�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�{�Dݻ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D��D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ȴAȸRAȮAț�AȓuAȑhAȑhAȏ\AȋDAȅAȁA�r�A�dZA�ZA�VA�O�A�K�A�G�A�C�A�;dA�5?A�+A��A�
=A���A��A���Aơ�A�9XA�?}A��7A��A��A�9XA��jA�n�A�n�A��A�9XA�;dA���A�$�A�I�A�hsA�n�A���A�ƨA���A��`A�A�A�Q�A��A���A��TA�A��DA�XA� �A��A�r�A��#A�;dA�A�1A��A��
A�$�A���A��wA�dZA�A���A��A�n�A���A��#A���A�E�A�
=A���A���A��A���A�/A��A��\A��\A��A���A�5?A��A��uA��A�JAp�A"�A~�`A~^5A{��Av~�Ar��Aq��Aq?}Ap�RAoG�Am`BAk+Aj1'Ah~�Ag7LAfn�Ae��Ac��Ab��AbQ�AbQ�AbM�AbA�Ab1AaƨA`I�A_�A_�
A_��A_��A_VA^�`A^  A]��A\�RA[��AZn�AY�TAY7LAX��AX{AVn�AT~�AS�ARZAP�`AO�AOƨAOl�AN�ANVAL�AJ��AI
=AH-AG|�AGG�AF��AEVAB�uAA��AAK�A?A>JA=��A<�/A;|�A:(�A8z�A7�A7/A5��A4^5A2�A2�uA2n�A1��A0v�A0  A/�^A/?}A.�`A.�uA.VA-�A,�uA+��A+S�A+VA*�RA*9XA)/A(  A&$�A$1A"��A!��A ��A bA|�A�jA�
Ap�AA�AĜA�AC�A�mA�uA��A�^AA1'A�yAI�AG�AA�+A�mA�^A�PA\)A
~�A�`A{AdZA
=A�9AZA(�A��A�A?}A��AffAC�A�A �A �\@��;@���@�+@���@�hs@���@� �@��F@��R@�@�`B@��9@� �@���@���@�@��@�\)@�R@�@�F@��@��@�\)@���@�j@�j@��@�w@�@�\)@���@�R@�$�@��@�7L@��@߾w@�"�@�=q@܃@��H@�V@�t�@��@��@Ұ!@�=q@���@��@�;d@���@�E�@���@��@��@ɑh@ȃ@�ƨ@�t�@�{@�`B@ě�@���@�-@��`@��@�\)@��\@��@�V@���@�Z@�l�@�V@���@��`@�Z@��F@��y@���@�X@��@�{@�G�@��9@��m@�;d@���@�r�@�n�@��#@�X@��`@�9X@��;@�\)@�;d@��H@�~�@��T@���@��j@��P@�J@�`B@�z�@���@��w@�@�v�@��@���@��/@�9X@��
@�dZ@�"�@�ff@��@�@��@��-@��@�Q�@���@���@��h@���@�S�@�33@�"�@�@��y@���@���@���@�v�@��T@�&�@�1'@��P@��@�t�@�S�@��y@�n�@�M�@�=q@���@��h@��@���@��@�X@�&�@��`@���@�`B@�G�@�X@�O�@�?}@�V@���@���@��9@��m@��\@��@�7L@�A�@�1@��w@�|�@�;d@�dZ@��@�-@��@��#@���@���@�p�@���@���@���@��h@���@��#@�@���@��h@�p�@�X@�X@�X@�O�@��D@��m@��;@���@��@���@���@���@��@�S�@�C�@�C�@�C�@�C�@�C�@�;d@�33@�+@��R@�n�@��#@��@�O�@��@�%@���@���@��j@��9@�j@�1'@� �@�b@�@�@}�h@|Z@{��@{��@{S�@{"�@zJ@y�@xĜ@x��@x�u@xbN@xA�@xA�@xA�@xA�@w�@w�w@w�@v�+@v5?@u�-@u/@t��@t�@s�F@s@q��@p��@p1'@o�@o��@o�@o|�@n��@n{@l��@l��@l�j@l�j@l��@lz�@lZ@lZ@lZ@lI�@l9X@l1@k�
@k��@k"�@j�@j�!@jn�@i�@i%@hA�@g�;@g|�@f{@e��@e��@e��@eO�@e/@e�@d�/@d(�@cdZ@cC�@c"�@b�@b��@b~�@a��@a&�@`�@`A�@`b@_�P@_
=@^��@^ff@^$�@^@]@]�h@]V@\�j@\��@\�D@\(�@[�
@[t�@[dZ@[S�@Z�H@Zn�@ZM�@Y�@Y��@Y��@Y��@Y��@Yx�@X��@Xb@W�@W�P@W\)@W+@W
=@V�y@V��@V$�@U�-@U�@U`B@U?}@T�/@TZ@T1@S��@R��@Q�7@Q&�@P��@P��@PA�@O��@Nȴ@Nff@M��@M`B@L�@L�@L�j@L(�@K�m@K�F@Ko@J��@J��@J��@J=q@J�@I�#@I�^@Ix�@IX@IG�@IG�@IG�@I7L@H�@HA�@G��@G;d@F�@F��@FV@F@F@E�@E�-@E�@D�@DZ@D(�@D1@C��@B�@B�@BJ@A��@A�@A��@A�^@A��@A�7@AG�@A%@@��@@��@@bN@@1'@@1'@@ �@?��@>��@>$�@>$�@=�T@=p�@=`B@=`B@=?}@=/@=/@=/@=/@=?}@=?}@=?}@=O�@=/@<j@<�@;33@9��@9G�@9�@8�`@8�9@8��@8r�@8A�@7�w@7;d@6�@65?@5�@5?}@4��@4I�@3�
@3�F@3�@3dZ@3S�@3o@2�@2~�@2M�@2=q@2-@1�#@1��@1��@1��@1�^@1�^@1��@1x�@1G�@0��@0Q�@/�@/l�@/K�@.�y@.v�@.E�@-�T@-�h@-��@-��@-�-@-p�@-V@,�j@,z�@+��@+��@+��@+�@+t�@+t�@+dZ@*�@*��@*~�@*^5@*=q@*�@)��@)�@)�#@)��@)��@)��@)��@)�@(��@'|�@'+@'
=@&ȴ@&v�@&V@&5?@%�@%��@%@%p�@$��@$��@$I�@#�m@#t�@#o@"��@"M�@"-@"-@"�@"J@!��@!�7@!hs@!hs@!X@!7L@ ��@ ��@ �`@ ��@ �9@ ��@ �@  �@�w@l�@+@��@ȴ@��@��@ff@5?@@��@�@�@�@?}@��@�@1@�@��@�\@^5@^5@�@�^@�7@X@�@��@�9@�@bN@b@��@|�@\)@\)@;d@+@
=@��@�+@��@�@O�@O�@/@V@��@�@�/@�/@��@��@�j@�@��@�D@Z@1@�m@�
@ƨ@�
@ƨ@ƨ@�
@�F@S�@"�@@��@�@J@J@�@�#@��@�^@x�@hs@G�@&�@�@��@Ĝ@�9@�u@r�@bN@A�@1'@  @�w@�P@�P@�P@�P@�P@�P@�P@l�@\)@K�@�@�@ȴ@��@��@V@$�@�@��@��@�h@�@O�@�@�/@�@j@Z@9X@9X@(�@��@�
@ƨ@�F@��@�@dZ@S�@@
��@
��@
�!@
�\@
n�@
^5@
-@
�@	��@	��@	�^@	��@	�7@	X@	G�@	%@��@Ĝ@�u@�u@�u@�u@�u@�u@r�@Q�@A�@A�@A�@b@�@�@|�@K�@�@ȴ@v�@5?@{@�-@p�@O�@?}@/@��@��@�D@z�@Z@Z@I�@9X@9X@(�@(�@�@1@�m@�m@�
@�F@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ȴAȸRAȮAț�AȓuAȑhAȑhAȏ\AȋDAȅAȁA�r�A�dZA�ZA�VA�O�A�K�A�G�A�C�A�;dA�5?A�+A��A�
=A���A��A���Aơ�A�9XA�?}A��7A��A��A�9XA��jA�n�A�n�A��A�9XA�;dA���A�$�A�I�A�hsA�n�A���A�ƨA���A��`A�A�A�Q�A��A���A��TA�A��DA�XA� �A��A�r�A��#A�;dA�A�1A��A��
A�$�A���A��wA�dZA�A���A��A�n�A���A��#A���A�E�A�
=A���A���A��A���A�/A��A��\A��\A��A���A�5?A��A��uA��A�JAp�A"�A~�`A~^5A{��Av~�Ar��Aq��Aq?}Ap�RAoG�Am`BAk+Aj1'Ah~�Ag7LAfn�Ae��Ac��Ab��AbQ�AbQ�AbM�AbA�Ab1AaƨA`I�A_�A_�
A_��A_��A_VA^�`A^  A]��A\�RA[��AZn�AY�TAY7LAX��AX{AVn�AT~�AS�ARZAP�`AO�AOƨAOl�AN�ANVAL�AJ��AI
=AH-AG|�AGG�AF��AEVAB�uAA��AAK�A?A>JA=��A<�/A;|�A:(�A8z�A7�A7/A5��A4^5A2�A2�uA2n�A1��A0v�A0  A/�^A/?}A.�`A.�uA.VA-�A,�uA+��A+S�A+VA*�RA*9XA)/A(  A&$�A$1A"��A!��A ��A bA|�A�jA�
Ap�AA�AĜA�AC�A�mA�uA��A�^AA1'A�yAI�AG�AA�+A�mA�^A�PA\)A
~�A�`A{AdZA
=A�9AZA(�A��A�A?}A��AffAC�A�A �A �\@��;@���@�+@���@�hs@���@� �@��F@��R@�@�`B@��9@� �@���@���@�@��@�\)@�R@�@�F@��@��@�\)@���@�j@�j@��@�w@�@�\)@���@�R@�$�@��@�7L@��@߾w@�"�@�=q@܃@��H@�V@�t�@��@��@Ұ!@�=q@���@��@�;d@���@�E�@���@��@��@ɑh@ȃ@�ƨ@�t�@�{@�`B@ě�@���@�-@��`@��@�\)@��\@��@�V@���@�Z@�l�@�V@���@��`@�Z@��F@��y@���@�X@��@�{@�G�@��9@��m@�;d@���@�r�@�n�@��#@�X@��`@�9X@��;@�\)@�;d@��H@�~�@��T@���@��j@��P@�J@�`B@�z�@���@��w@�@�v�@��@���@��/@�9X@��
@�dZ@�"�@�ff@��@�@��@��-@��@�Q�@���@���@��h@���@�S�@�33@�"�@�@��y@���@���@���@�v�@��T@�&�@�1'@��P@��@�t�@�S�@��y@�n�@�M�@�=q@���@��h@��@���@��@�X@�&�@��`@���@�`B@�G�@�X@�O�@�?}@�V@���@���@��9@��m@��\@��@�7L@�A�@�1@��w@�|�@�;d@�dZ@��@�-@��@��#@���@���@�p�@���@���@���@��h@���@��#@�@���@��h@�p�@�X@�X@�X@�O�@��D@��m@��;@���@��@���@���@���@��@�S�@�C�@�C�@�C�@�C�@�C�@�;d@�33@�+@��R@�n�@��#@��@�O�@��@�%@���@���@��j@��9@�j@�1'@� �@�b@�@�@}�h@|Z@{��@{��@{S�@{"�@zJ@y�@xĜ@x��@x�u@xbN@xA�@xA�@xA�@xA�@w�@w�w@w�@v�+@v5?@u�-@u/@t��@t�@s�F@s@q��@p��@p1'@o�@o��@o�@o|�@n��@n{@l��@l��@l�j@l�j@l��@lz�@lZ@lZ@lZ@lI�@l9X@l1@k�
@k��@k"�@j�@j�!@jn�@i�@i%@hA�@g�;@g|�@f{@e��@e��@e��@eO�@e/@e�@d�/@d(�@cdZ@cC�@c"�@b�@b��@b~�@a��@a&�@`�@`A�@`b@_�P@_
=@^��@^ff@^$�@^@]@]�h@]V@\�j@\��@\�D@\(�@[�
@[t�@[dZ@[S�@Z�H@Zn�@ZM�@Y�@Y��@Y��@Y��@Y��@Yx�@X��@Xb@W�@W�P@W\)@W+@W
=@V�y@V��@V$�@U�-@U�@U`B@U?}@T�/@TZ@T1@S��@R��@Q�7@Q&�@P��@P��@PA�@O��@Nȴ@Nff@M��@M`B@L�@L�@L�j@L(�@K�m@K�F@Ko@J��@J��@J��@J=q@J�@I�#@I�^@Ix�@IX@IG�@IG�@IG�@I7L@H�@HA�@G��@G;d@F�@F��@FV@F@F@E�@E�-@E�@D�@DZ@D(�@D1@C��@B�@B�@BJ@A��@A�@A��@A�^@A��@A�7@AG�@A%@@��@@��@@bN@@1'@@1'@@ �@?��@>��@>$�@>$�@=�T@=p�@=`B@=`B@=?}@=/@=/@=/@=/@=?}@=?}@=?}@=O�@=/@<j@<�@;33@9��@9G�@9�@8�`@8�9@8��@8r�@8A�@7�w@7;d@6�@65?@5�@5?}@4��@4I�@3�
@3�F@3�@3dZ@3S�@3o@2�@2~�@2M�@2=q@2-@1�#@1��@1��@1��@1�^@1�^@1��@1x�@1G�@0��@0Q�@/�@/l�@/K�@.�y@.v�@.E�@-�T@-�h@-��@-��@-�-@-p�@-V@,�j@,z�@+��@+��@+��@+�@+t�@+t�@+dZ@*�@*��@*~�@*^5@*=q@*�@)��@)�@)�#@)��@)��@)��@)��@)�@(��@'|�@'+@'
=@&ȴ@&v�@&V@&5?@%�@%��@%@%p�@$��@$��@$I�@#�m@#t�@#o@"��@"M�@"-@"-@"�@"J@!��@!�7@!hs@!hs@!X@!7L@ ��@ ��@ �`@ ��@ �9@ ��@ �@  �@�w@l�@+@��@ȴ@��@��@ff@5?@@��@�@�@�@?}@��@�@1@�@��@�\@^5@^5@�@�^@�7@X@�@��@�9@�@bN@b@��@|�@\)@\)@;d@+@
=@��@�+@��@�@O�@O�@/@V@��@�@�/@�/@��@��@�j@�@��@�D@Z@1@�m@�
@ƨ@�
@ƨ@ƨ@�
@�F@S�@"�@@��@�@J@J@�@�#@��@�^@x�@hs@G�@&�@�@��@Ĝ@�9@�u@r�@bN@A�@1'@  @�w@�P@�P@�P@�P@�P@�P@�P@l�@\)@K�@�@�@ȴ@��@��@V@$�@�@��@��@�h@�@O�@�@�/@�@j@Z@9X@9X@(�@��@�
@ƨ@�F@��@�@dZ@S�@@
��@
��@
�!@
�\@
n�@
^5@
-@
�@	��@	��@	�^@	��@	�7@	X@	G�@	%@��@Ĝ@�u@�u@�u@�u@�u@�u@r�@Q�@A�@A�@A�@b@�@�@|�@K�@�@ȴ@v�@5?@{@�-@p�@O�@?}@/@��@��@�D@z�@Z@Z@I�@9X@9X@(�@(�@�@1@�m@�m@�
@�F@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BF�BE�BF�BG�BH�BH�BG�BG�BG�BG�BF�BF�BK�BO�BP�BR�BS�BVBYB]/B`BBcTBffBffBe`B`BBQ�BE�BXBx�Bz�BK�BQ�BXB}�Bm�B]/BbNBdZBjBH�B:^B�BhB7LB6FB%�B�B�BJB
=B��BB��B��B��B��B��B�yB�#B�B�
B��BB��B��B�Bz�B{�B�B~�Bu�Bn�B\)BVBF�BQ�BH�B5?B2-B)�B�B�BhB
��B
�B
�qB
�-B
�dB
�?B
��B
�PB
u�B
n�B
v�B
t�B
l�B
[#B
7LB
+B	��B
hB
{B
PB	��B	�ZB	��B	�B	��B	��B	��B	ǮB	�XB	�^B	ƨB	ɺB	ǮB	ŢB	��B	�jB	�B	�LB	�dB	�^B	�FB	�B	�!B	��B	��B	��B	��B	��B	��B	�{B	�uB	�DB	t�B	q�B	iyB	n�B	dZB	cTB	hsB	bNB	XB	S�B	F�B	2-B	.B	33B	5?B	2-B	(�B	�B	B	B	+B�B�B��B�yB�B�BȴB��B��BǮB�qB�wBĜBŢB�jB�9B�dB�qB�^B�RB�FB�9B�B��B��B�B�B��B��B�oB�JBx�By�Bt�Bz�Bw�By�Bu�Bu�Bn�Bl�B\)BP�BcTB`BBYBN�BD�B=qBC�BJ�BE�B<jBF�BC�BL�BJ�BG�BM�BK�BH�B?}B1'B?}BA�BD�BF�BD�BF�BD�BB�BB�B>wB:^B5?B2-B6FB%�B0!B+B)�B1'B;dB9XB5?B8RB49B49B5?B49B49B5?B0!B.B(�B1'B0!B(�B$�B#�B,B(�B+B/B7LB5?B8RB8RB8RB6FB7LB33B49B33B1'B1'B1'B.B&�B$�B�B �B"�B�B�B&�B#�B(�B'�B+B)�B#�B'�B.B7LB33B49B7LB/B49B49B49B0!B5?B=qB@�B=qBA�B>wBF�BF�BE�BH�BM�BM�BP�BP�BO�BP�BVBR�BN�B`BBbNBbNBbNB_;B_;BaHBr�Bv�Bx�By�B}�B}�B�B�B� B~�B�B}�B�B�B�\B�oB��B��B��B��B��B��B��B��B��B�B�'B�-B�RB�^B�dB�^B�FB�^B�FBÖBɺBŢBÖB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�5B�TB�TB�ZB�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B��B��B��B��B	B	B	bB	uB	�B	�B	�B	�B	�B	%�B	-B	.B	/B	2-B	7LB	9XB	9XB	>wB	F�B	E�B	E�B	F�B	G�B	G�B	H�B	I�B	J�B	I�B	G�B	H�B	M�B	P�B	T�B	XB	ZB	[#B	[#B	]/B	aHB	cTB	cTB	cTB	cTB	cTB	cTB	cTB	aHB	ffB	gmB	o�B	t�B	u�B	v�B	w�B	w�B	x�B	x�B	w�B	y�B	{�B	|�B	{�B	}�B	~�B	�%B	�VB	�bB	�oB	�oB	�hB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�3B	�FB	�?B	�FB	�^B	�wB	��B	B	B	��B	��B	�}B	��B	ǮB	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�5B	�;B	�;B	�5B	�5B	�5B	�5B	�HB	�ZB	�ZB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
B
B
B
%B
1B
1B
+B
1B
	7B
1B

=B
JB
JB
DB
PB
PB
VB
VB
VB
\B
\B
VB
VB
JB
VB
VB
VB
\B
bB
hB
oB
{B
{B
uB
uB
oB
oB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
(�B
+B
+B
+B
+B
)�B
+B
)�B
+B
-B
,B
,B
-B
.B
.B
.B
.B
.B
-B
-B
.B
0!B
1'B
33B
49B
33B
33B
5?B
5?B
5?B
7LB
7LB
6FB
5?B
49B
49B
49B
49B
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
9XB
8RB
8RB
7LB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
=qB
>wB
?}B
?}B
?}B
@�B
B�B
B�B
E�B
E�B
E�B
E�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
F�B
F�B
E�B
F�B
G�B
H�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
O�B
P�B
O�B
N�B
M�B
L�B
Q�B
Q�B
O�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
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
YB
YB
ZB
ZB
ZB
YB
XB
ZB
XB
[#B
]/B
^5B
^5B
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
^5B
_;B
`BB
aHB
aHB
aHB
`BB
`BB
_;B
^5B
_;B
`BB
_;B
`BB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
iyB
hsB
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
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BF�BE�BF�BG�BH�BH�BG�BG�BG�BG�BF�BF�BK�BO�BQ BR�BS�BVBY1B]IB`\BcnBf�Bf�Be�Ba|BU�BK�B_�B}B�BP�BUBZ�B~�Bp!B`�Be�Bf�Bl=BL�B>�B$tBgB7�B7B($B�B�B�B0B��B�B�2B��B��B�jB��B�6B�dBچB�yB�VB�gB��B��B��B~B}�B��B�Bv�Bo�B]�BW�BH�BR�BJ	B7�B3hB+�B�B�B�B
��B
�B
��B
�B
��B
�`B
��B
�HB
y>B
p�B
wfB
u%B
mCB
\�B
;0B
B	��B
�B
�B
<B	��B	��B	�}B	�QB	��B	�BB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�"B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�YB	�MB	�B	��B	v�B	s�B	kQB	o�B	fB	dZB	h�B	cB	YB	T�B	H�B	4�B	0UB	49B	6B	2�B	*0B	�B	�B	YB	�B�UB��B�tB��B�B��B��B� B��BɠB�.B� B�B�B��B��B�B��B�0B��B��B��B�=B�8B�B�wB��B��B��B�B�"B{�B|PBv�B|By	Bz�Bv�Bv�Bo�Bm�B^�BSuBd�BabBZ�BP�BF�B?�BEBK�BF�B>BG�BD�BMPBKxBH�BN"BL0BIlB@�B3hB@�BBuBESBG+BESBG+BEBCBC-B?HB;dB6�B3MB7B(
B1AB,�B+�B2B;�B9�B5�B8�B4�B4�B5�B4�B4�B5�B0�B/B*KB1�B0�B)�B&2B%,B,�B*B,B/�B7�B5�B8�B8�B8�B6�B7�B3�B4�B3�B1�B1�B1�B.�B(>B&2B B!�B#�B!B#B'RB$�B)�B(�B+kB*�B%,B)B.�B7�B4B4�B7�B0;B4�B4�B4�B1[B6FB>(BA B>(BBB?.BF�BG_BFtBI�BNVBN�BQ�BQ�BP�BQ�BV�BS�BPbB`�Bb�Bc BcB`\B`\Bb�BsBw2By>Bz^B~]B~wB�AB�oB�iB}B�oB~�B��B�B��B�@B�B��B�\B�;B�ZB�RB�zB�sB�eB�qB�vB��B��B�xB��B��B��B��B��B�3B�	B�YBĜB��B�B�2B�2B�,B�2B�2B�FB�hB�vB�vB�NB�$B�+B�EB�yBچB�jB�nB�B�B�B�yB�B�B��B��B��B��B�B��B��B�B�<B�B	 B�VB��B��B�}B	�B	�B	�B	�B	�B	�B	�B	/B	 \B	&B	-)B	./B	/OB	2GB	72B	9XB	9�B	>�B	FtB	E�B	E�B	F�B	G�B	G�B	H�B	I�B	J�B	I�B	H1B	IB	M�B	Q B	UB	X+B	ZB	[=B	[=B	]dB	abB	cTB	cTB	cTB	c�B	cnB	cnB	c�B	a�B	f�B	h
B	o�B	t�B	u�B	v�B	w�B	w�B	x�B	x�B	x8B	z*B	|B	}B	|6B	~BB	�B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�CB	�)B	�qB	�IB	�vB	�[B	�aB	�nB	��B	�zB	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�<B	�B	�B	�VB	�B	�1B	�1B	�1B	�=B	�=B	�kB	�B	چB	�OB	�VB	�VB	�OB	�jB	�jB	ޞB	�|B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	��B	�B	��B	�B	�	B	��B	�B	�>B	�DB	�B
 B
 B
 OB
UB
UB
gB
MB
SB
tB
1B
fB
_B
KB
	RB
�B

XB
dB
JB
xB
�B
jB
pB
pB
pB
vB
\B
pB
pB
�B
pB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
�B
!B
�B
;B
B
�B
"�B
$B
#�B
#�B
#�B
$�B
$&B
%B
%,B
%,B
%,B
'B
'B
'B
)*B
+6B
+6B
+6B
+6B
*B
+B
*0B
+B
-B
,"B
,"B
-B
./B
.B
.B
.B
./B
-)B
-)B
.IB
0oB
1[B
3hB
4TB
3hB
3�B
5ZB
5ZB
5ZB
7LB
7LB
6FB
5ZB
4nB
4TB
4TB
4nB
6`B
7LB
8lB
8RB
8RB
8lB
8�B
9rB
:xB
:�B
:�B
:xB
:xB
:^B
:xB
:^B
:xB
:^B
9rB
8�B
8�B
7�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
=�B
>�B
?�B
?�B
?�B
@�B
B�B
B�B
E�B
E�B
E�B
E�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
F�B
F�B
E�B
F�B
G�B
H�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
O�B
P�B
O�B
N�B
NB
MB
R B
R B
P.B
R�B
SB
SB
S@B
TB
TB
UB
UB
VB
W$B
W$B
W$B
W$B
WYB
YB
Y1B
ZB
Z7B
ZB
Y1B
XEB
Z7B
XyB
[=B
]IB
^OB
^OB
]IB
^5B
^5B
^5B
_;B
_;B
_VB
_;B
_;B
_;B
_;B
^jB
^jB
_VB
`BB
aHB
abB
aHB
`\B
`BB
_pB
^OB
_VB
`\B
_pB
`vB
cTB
cnB
c�B
cTB
cTB
cnB
cnB
cnB
dtB
cnB
dtB
d�B
dtB
dZB
dtB
e`B
ezB
ezB
e�B
ezB
ezB
f�B
gmB
g�B
gmB
gmB
gmB
g�B
gmB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
f�B
g�B
g�B
h�B
h�B
hsB
h�B
h�B
g�B
iyB
i�B
h�B
jB
j�B
k�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811290034102018112900341020181129003410201811290200152018112902001520181129020015201811300022152018113000221520181130002215  JA  ARFMdecpA19c                                                                20181125093625  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181125003629  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181125003633  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181125003633  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181125003634  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181125003634  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181125003634  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181125003634  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181125003639  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181125003639                      G�O�G�O�G�O�                JA  ARUP                                                                        20181125005718                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181125153451  CV  JULD            G�O�G�O�Fę�                JM  ARCAJMQC2.0                                                                 20181128153410  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181128153410  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181128170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181129152215  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                