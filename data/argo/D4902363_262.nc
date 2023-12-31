CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-24T00:42:11Z creation;2018-07-24T00:42:16Z conversion to V3.1;2019-12-19T07:36:57Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180724004211  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_262                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�s�A���1   @�s��l @9�rGE8��dT��7��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD:�D:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�Dׁ�D׾�D��D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D��RD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA�dZA�dZA�dZA�dZA�bNA�`BA�bNA�bNA�bNA�\)A�VA�S�A�Q�A�M�A��A��/A�|�A��Aȉ7A��A�"�A���A���A��A��FA�p�A���A��-A�  A���A���A���A�$�A��A��TA�O�A���A�C�A��A���A�r�A��HA�?}A��+A�v�A�v�A�VA�XA��A�5?A��A�9XA���A���A�dZA�
=A�E�A�v�A���A�=qA��^A���A��A�bNA�?}A�  A�ĜA�|�A�G�A���A�dZA��;A�hsA��#A�O�A��/A��jA���A�t�A��A�bNA��PA�
=A��!A�;dA���A�C�A���A��+A��A�XA���A�hsA�A}O�Ax�HAv�At�RAsƨAr��AqO�Ao��An�yAm�7AlbNAkAk7LAjz�Ai?}AhAf�AfAd�Adv�AdAc�7Ab�9Abn�Aa�hAa�A`��A_��A^��A]�;A[��AZ�AY��AX�yAXv�AW�hAV��AUXATI�AR�/AQC�AO�AOXAN�9AMx�AL�+AK�AJ1AH��AH�jAH�AGAF�+AE33AC�TACVAB��AB5?AA�TA@��A@9XA@JA>�jA<v�A;|�A:�A9?}A8�DA7��A6�HA6�+A5|�A3A2�A2$�A1�wA1/A/�;A.1A,��A,��A+�mA*I�A(�A(-A(  A'��A'C�A&��A$ȴA$1A#��A#x�A#?}A"��A!�#A VAC�A{A�!AQ�A�#A7LAn�A33Ar�A�hA�mA/A�jA�AXAffAG�A�#A"�A�+A��Ax�A7LA	�-A��AI�A��A�yA�At�A`BA �AG�A�A��AjA$�A��A �HA ^5A J@�K�@�j@��@��;@��@��/@�C�@��@��@��-@��@�=q@���@�b@�S�@�V@�Ĝ@���@���@��@ߕ�@ݩ�@� �@���@�X@�Ĝ@��
@ָR@ԛ�@�|�@���@�E�@�@��`@�(�@� �@�|�@θR@���@��@�Z@�Q�@�(�@�dZ@�n�@�-@ɺ^@�p�@��@��
@�o@��@�I�@��@�j@���@��P@�;d@��R@�`B@��;@�t�@���@���@��@�S�@��H@��^@���@��!@�@��`@�t�@�V@��@��@�1@�S�@�^5@��@�z�@�ƨ@�S�@�ff@��#@�p�@���@��@��@�C�@���@��!@�~�@�{@���@���@�Q�@���@���@�t�@�S�@�;d@�"�@���@�ȴ@�ff@�@��/@�z�@��m@�K�@�ȴ@��@�Ĝ@�Z@� �@�b@��P@��+@���@�p�@�?}@�V@���@�bN@�9X@��m@���@�S�@���@��@���@���@�x�@�V@��j@�  @�33@��@���@��\@��h@��j@���@��w@��@�dZ@���@�ȴ@�E�@�%@�j@�A�@�(�@�1@��@�S�@�33@��@���@�ȴ@���@��!@�~�@��@���@��7@��`@���@�1'@�1@��
@���@��@��@�\)@�;d@��y@��!@�v�@�=q@�$�@�$�@��@�@��T@�@��-@��h@�x�@�p�@�X@�&�@��@��u@�bN@� �@�@�;@|�@~��@~$�@}��@}�-@}��@}p�@|��@|�@|�@|�D@|9X@{�m@{�F@{�@{C�@{@z�\@z^5@z=q@y��@y��@yx�@yX@y�@x��@x��@x��@xQ�@xA�@x1'@xb@w�;@w�@w�P@vȴ@u��@u?}@u�@t��@t��@s�m@s�F@s��@s"�@r��@r�\@r-@r�@q��@q�@q��@q��@qX@q7L@p�`@p�9@p��@pbN@pb@oK�@n��@m�T@mO�@l��@l9X@l�@kdZ@k"�@j�!@j=q@j�@i�@i�^@i��@iX@h�`@h�u@hA�@h1'@g��@g�P@f��@fE�@e�@e�-@eO�@dI�@c�
@c��@cS�@b�@b��@a��@ax�@`�9@_�@_�w@_+@^�R@^5?@]@]�h@]p�@]p�@]O�@\�/@\�D@\1@[�F@[S�@[@Z��@Z��@Z��@ZM�@ZJ@Y�#@Y�7@Y&�@X��@X�9@X �@W�w@W|�@W;d@V��@Vȴ@Vv�@V5?@V@U�@U��@U�h@T��@T9X@T1@S�
@S�F@S��@SS�@S@R�!@RM�@Q��@Q�^@Qhs@Q&�@Q%@PĜ@Pr�@P �@O��@O;d@N��@NV@N{@M�T@M�-@Mp�@M�@M�@L��@L�/@Lz�@L�@K��@K"�@J��@J��@Jn�@I�7@I7L@H��@H�9@H�u@H�@HA�@Hb@G�;@GK�@Fȴ@Fv�@FV@F{@E@E��@Ep�@EO�@E/@EV@D��@D�j@D�D@DZ@D�@Cƨ@C�F@C�F@CS�@B��@B=q@A�@A��@A7L@@��@@bN@?��@?K�@>ȴ@>�+@>v�@>ff@>5?@=�@=�-@=�@=O�@<��@<��@<j@<I�@<�@;��@;ƨ@;�@;o@:��@:~�@:n�@:^5@:�@9�#@9hs@9�@8�@8b@7�P@7\)@7K�@6�R@6ff@6E�@5�-@5/@4�j@4(�@3�@3"�@2��@2�!@2n�@2J@1�@1�^@1x�@1X@1&�@1�@0��@0�`@0�`@0�9@0r�@0Q�@0  @/�;@/��@/�w@/|�@.�@.v�@.{@-�T@-��@-�-@-`B@-�@,�@,�@,I�@,�@+�m@+�F@+t�@+C�@+"�@+o@*�H@*��@*��@*��@*�!@*M�@)��@)�#@)��@)hs@(�`@(  @&E�@&{@&@%�-@%`B@$�/@$�D@$�@#�m@#ƨ@#��@#C�@#@"��@"M�@"=q@"-@"-@"J@!��@!��@!�7@!hs@!X@!�@ �9@ ��@ �@ r�@ Q�@   @�@�P@l�@K�@;d@+@��@�R@��@V@{@@��@p�@?}@�@�/@�@�D@z�@z�@j@9X@(�@�m@�F@��@S�@o@��@��@~�@M�@J@��@�^@��@��@�7@7L@%@Ĝ@��@�@bN@Q�@�;@�w@�@��@l�@\)@K�@;d@�@�R@�+@ff@5?@$�@{@�@�-@`B@/@�@�@��@z�@Z@I�@(�@1@�m@�
@�F@��@S�@o@@�@�H@�H@�H@��@�\@J@�^@�7@x�@hs@7L@�@%@�`@�`@�@1'@�;@��@�w@�w@�w@��@;d@+@ȴ@�R@��@��@�+@ff@ff@5?@�-@p�@`B@O�@/@V@�/@z�@�@1@��@��@�
@��@t�@C�@"�@o@
�@
��@
��@
�!@
�\@
^5@
M�@
=q@
�@
�@
J@
J@	��@	�@	��@	��@	x�@	G�@	7L@	&�@	%@��@�`@��@�@r�@r�@r�@bN@Q�@A�@ �@�@�;@��@�w@�w@�w@��@�P@|�@
=@�@�R@��@ff@E�@5?@�T@�-@��@�h@`B@?}@V@�j@z�@Z@9X@9X@(�@1@�
@��@dZ@"�@o@��@~�@^5@=q@J@�@��@�^@��@��@�7@�7@�7@x�@�@ �`@ Ĝ@ �9@ �@ r�@ bN@ bN@ Q�@  �?��;?��w?���?��?���?��?��?�v�?�p�?��?���?�I�?�1?�1?�ƨ?�ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA�dZA�dZA�dZA�dZA�bNA�`BA�bNA�bNA�bNA�\)A�VA�S�A�Q�A�M�A��A��/A�|�A��Aȉ7A��A�"�A���A���A��A��FA�p�A���A��-A�  A���A���A���A�$�A��A��TA�O�A���A�C�A��A���A�r�A��HA�?}A��+A�v�A�v�A�VA�XA��A�5?A��A�9XA���A���A�dZA�
=A�E�A�v�A���A�=qA��^A���A��A�bNA�?}A�  A�ĜA�|�A�G�A���A�dZA��;A�hsA��#A�O�A��/A��jA���A�t�A��A�bNA��PA�
=A��!A�;dA���A�C�A���A��+A��A�XA���A�hsA�A}O�Ax�HAv�At�RAsƨAr��AqO�Ao��An�yAm�7AlbNAkAk7LAjz�Ai?}AhAf�AfAd�Adv�AdAc�7Ab�9Abn�Aa�hAa�A`��A_��A^��A]�;A[��AZ�AY��AX�yAXv�AW�hAV��AUXATI�AR�/AQC�AO�AOXAN�9AMx�AL�+AK�AJ1AH��AH�jAH�AGAF�+AE33AC�TACVAB��AB5?AA�TA@��A@9XA@JA>�jA<v�A;|�A:�A9?}A8�DA7��A6�HA6�+A5|�A3A2�A2$�A1�wA1/A/�;A.1A,��A,��A+�mA*I�A(�A(-A(  A'��A'C�A&��A$ȴA$1A#��A#x�A#?}A"��A!�#A VAC�A{A�!AQ�A�#A7LAn�A33Ar�A�hA�mA/A�jA�AXAffAG�A�#A"�A�+A��Ax�A7LA	�-A��AI�A��A�yA�At�A`BA �AG�A�A��AjA$�A��A �HA ^5A J@�K�@�j@��@��;@��@��/@�C�@��@��@��-@��@�=q@���@�b@�S�@�V@�Ĝ@���@���@��@ߕ�@ݩ�@� �@���@�X@�Ĝ@��
@ָR@ԛ�@�|�@���@�E�@�@��`@�(�@� �@�|�@θR@���@��@�Z@�Q�@�(�@�dZ@�n�@�-@ɺ^@�p�@��@��
@�o@��@�I�@��@�j@���@��P@�;d@��R@�`B@��;@�t�@���@���@��@�S�@��H@��^@���@��!@�@��`@�t�@�V@��@��@�1@�S�@�^5@��@�z�@�ƨ@�S�@�ff@��#@�p�@���@��@��@�C�@���@��!@�~�@�{@���@���@�Q�@���@���@�t�@�S�@�;d@�"�@���@�ȴ@�ff@�@��/@�z�@��m@�K�@�ȴ@��@�Ĝ@�Z@� �@�b@��P@��+@���@�p�@�?}@�V@���@�bN@�9X@��m@���@�S�@���@��@���@���@�x�@�V@��j@�  @�33@��@���@��\@��h@��j@���@��w@��@�dZ@���@�ȴ@�E�@�%@�j@�A�@�(�@�1@��@�S�@�33@��@���@�ȴ@���@��!@�~�@��@���@��7@��`@���@�1'@�1@��
@���@��@��@�\)@�;d@��y@��!@�v�@�=q@�$�@�$�@��@�@��T@�@��-@��h@�x�@�p�@�X@�&�@��@��u@�bN@� �@�@�;@|�@~��@~$�@}��@}�-@}��@}p�@|��@|�@|�@|�D@|9X@{�m@{�F@{�@{C�@{@z�\@z^5@z=q@y��@y��@yx�@yX@y�@x��@x��@x��@xQ�@xA�@x1'@xb@w�;@w�@w�P@vȴ@u��@u?}@u�@t��@t��@s�m@s�F@s��@s"�@r��@r�\@r-@r�@q��@q�@q��@q��@qX@q7L@p�`@p�9@p��@pbN@pb@oK�@n��@m�T@mO�@l��@l9X@l�@kdZ@k"�@j�!@j=q@j�@i�@i�^@i��@iX@h�`@h�u@hA�@h1'@g��@g�P@f��@fE�@e�@e�-@eO�@dI�@c�
@c��@cS�@b�@b��@a��@ax�@`�9@_�@_�w@_+@^�R@^5?@]@]�h@]p�@]p�@]O�@\�/@\�D@\1@[�F@[S�@[@Z��@Z��@Z��@ZM�@ZJ@Y�#@Y�7@Y&�@X��@X�9@X �@W�w@W|�@W;d@V��@Vȴ@Vv�@V5?@V@U�@U��@U�h@T��@T9X@T1@S�
@S�F@S��@SS�@S@R�!@RM�@Q��@Q�^@Qhs@Q&�@Q%@PĜ@Pr�@P �@O��@O;d@N��@NV@N{@M�T@M�-@Mp�@M�@M�@L��@L�/@Lz�@L�@K��@K"�@J��@J��@Jn�@I�7@I7L@H��@H�9@H�u@H�@HA�@Hb@G�;@GK�@Fȴ@Fv�@FV@F{@E@E��@Ep�@EO�@E/@EV@D��@D�j@D�D@DZ@D�@Cƨ@C�F@C�F@CS�@B��@B=q@A�@A��@A7L@@��@@bN@?��@?K�@>ȴ@>�+@>v�@>ff@>5?@=�@=�-@=�@=O�@<��@<��@<j@<I�@<�@;��@;ƨ@;�@;o@:��@:~�@:n�@:^5@:�@9�#@9hs@9�@8�@8b@7�P@7\)@7K�@6�R@6ff@6E�@5�-@5/@4�j@4(�@3�@3"�@2��@2�!@2n�@2J@1�@1�^@1x�@1X@1&�@1�@0��@0�`@0�`@0�9@0r�@0Q�@0  @/�;@/��@/�w@/|�@.�@.v�@.{@-�T@-��@-�-@-`B@-�@,�@,�@,I�@,�@+�m@+�F@+t�@+C�@+"�@+o@*�H@*��@*��@*��@*�!@*M�@)��@)�#@)��@)hs@(�`@(  @&E�@&{@&@%�-@%`B@$�/@$�D@$�@#�m@#ƨ@#��@#C�@#@"��@"M�@"=q@"-@"-@"J@!��@!��@!�7@!hs@!X@!�@ �9@ ��@ �@ r�@ Q�@   @�@�P@l�@K�@;d@+@��@�R@��@V@{@@��@p�@?}@�@�/@�@�D@z�@z�@j@9X@(�@�m@�F@��@S�@o@��@��@~�@M�@J@��@�^@��@��@�7@7L@%@Ĝ@��@�@bN@Q�@�;@�w@�@��@l�@\)@K�@;d@�@�R@�+@ff@5?@$�@{@�@�-@`B@/@�@�@��@z�@Z@I�@(�@1@�m@�
@�F@��@S�@o@@�@�H@�H@�H@��@�\@J@�^@�7@x�@hs@7L@�@%@�`@�`@�@1'@�;@��@�w@�w@�w@��@;d@+@ȴ@�R@��@��@�+@ff@ff@5?@�-@p�@`B@O�@/@V@�/@z�@�@1@��@��@�
@��@t�@C�@"�@o@
�@
��@
��@
�!@
�\@
^5@
M�@
=q@
�@
�@
J@
J@	��@	�@	��@	��@	x�@	G�@	7L@	&�@	%@��@�`@��@�@r�@r�@r�@bN@Q�@A�@ �@�@�;@��@�w@�w@�w@��@�P@|�@
=@�@�R@��@ff@E�@5?@�T@�-@��@�h@`B@?}@V@�j@z�@Z@9X@9X@(�@1@�
@��@dZ@"�@o@��@~�@^5@=q@J@�@��@�^@��@��@�7@�7@�7@x�@�@ �`@ Ĝ@ �9@ �@ r�@ bN@ bN@ Q�@  �?��;?��w?���?��?���?��?��?�v�?�p�?��?���?�I�?�1?�1?�ƨ?�ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�mB�ZB�5B��B��BjBy�B�B9XBH�BaHBVB1'B#�BQ�BW
BG�B\)BYBdZBZBS�B=qBA�BH�BYBXB9XB'�B5?B�B�B��B
=BB1BB�B�B�`BŢB�B�B�VB�B��B�PBy�B�B�+B�=B�1B�By�Br�BdZBZBQ�BA�BQ�B7LB�B7LB7LB/B�B{BBB
��B
�B
ÖB
ɺB
�9B
�7B
�B
�PB
v�B
YB
M�B
9XB
B
B
bB
oB

=B
B	��B
B	��B	�B	�B	�B	�sB	�#B	�B	�B	��B	��B	��B	��B	��B	��B	ƨB	�qB	�XB	�^B	�B	��B	��B	�%B	�B	�B	{�B	{�B	r�B	ffB	^5B	R�B	L�B	?}B	<jB	A�B	;dB	.B	,B	&�B	�B	�B	&�B	�B	�B	%B��B��B	  B	B	  B��B�B�B�B�)BB��B��BǮBȴBŢBÖBÖB�LB�B�B�qB�XB�!B��B�VB��B��B��B�7B�DB�oB��B�{B�\B�Bt�B� B�+B�=B�B{�Bs�BcTBjBaHB^5Bl�BgmBaHB[#BP�BVBM�BD�BQ�BS�BN�BI�BC�B=qB>wB@�BG�B>wB9XB#�B!�B9XB6FB5?B49B-B5?B7LB)�B%�B5?B49B0!B1'B,B&�B(�B+B#�B{BhB�B�B�B�B{BbB%BPBVBbB�B�BhBJB+B	7BoB{B1B+BVB1BoBPB
=BBhB�B�B�B{B�B�B�B�B�BhB�B!�B%�B!�B�B&�B&�B+B,B&�B'�B%�B�B�B(�B5?B9XB8RB33B.B.B9XB5?B0!B9XB?}B=qB8RB0!BA�B@�BC�BA�BE�BH�BQ�BR�BQ�BP�BT�BVB]/B_;B_;BcTBgmBffBk�BjBhsBp�Bt�Bs�Bq�Bq�Br�Bv�Bz�B}�B� B�B�B�B� B� B|�B{�Bz�B�B�B�B�%B�B�B�oB��B��B�hB�bB��B��B��B��B��B�B�B�B�B�'B�B�9B�wB�}B�wB�qB�}B�wBÖB��B��B��BȴB��B��B�NB�TB�TB�mB�B�sB�fB�B��B��B��B��B��B	B	B	B	+B	%B	%B	B	B	1B	
=B		7B	\B	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	(�B	(�B	'�B	(�B	)�B	,B	,B	.B	/B	/B	.B	0!B	0!B	5?B	8RB	;dB	=qB	<jB	<jB	>wB	F�B	K�B	K�B	K�B	K�B	M�B	P�B	O�B	O�B	P�B	S�B	T�B	VB	XB	YB	]/B	_;B	^5B	`BB	cTB	dZB	dZB	ffB	ffB	gmB	hsB	jB	k�B	jB	k�B	k�B	k�B	iyB	l�B	t�B	x�B	x�B	y�B	z�B	� B	�B	�B	�B	�%B	�+B	�=B	�DB	�DB	�DB	�JB	�PB	�\B	�bB	�hB	�oB	�oB	�oB	�hB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�'B	�3B	�?B	�LB	�XB	�XB	�^B	�^B	�dB	�}B	B	ÖB	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�;B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
B
B
1B
	7B
DB
DB
JB
DB
DB
JB

=B
JB
VB
\B
\B
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
#�B
#�B
#�B
"�B
"�B
!�B
#�B
"�B
#�B
$�B
&�B
'�B
%�B
&�B
(�B
&�B
&�B
(�B
(�B
)�B
,B
-B
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
1'B
1'B
2-B
33B
2-B
2-B
0!B
2-B
49B
49B
5?B
5?B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
9XB
8RB
8RB
9XB
9XB
8RB
6FB
5?B
33B
>wB
?}B
>wB
=qB
=qB
?}B
?}B
A�B
B�B
A�B
@�B
A�B
A�B
B�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
F�B
F�B
F�B
F�B
E�B
F�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
I�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
P�B
O�B
P�B
P�B
Q�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
R�B
T�B
T�B
VB
VB
VB
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
YB
XB
XB
YB
YB
YB
YB
XB
YB
ZB
[#B
[#B
[#B
ZB
ZB
YB
XB
ZB
[#B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
`BB
aHB
`BB
aHB
`BB
`BB
_;B
^5B
`BB
bNB
bNB
aHB
aHB
aHB
`BB
aHB
cTB
dZB
dZB
cTB
cTB
e`B
dZB
e`B
ffB
e`B
e`B
ffB
ffB
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
jB
iyB
iyB
iyB
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
l�B
l�B
l�B
k�B
k�B
k�B
jB
l�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
p�B
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
t�B
s�B
s�B
r�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B��B�B�B�B�B��B�BߊB�[B��BuB��B	B;JBJ�BbBW�B5%B(
BS[BYKBJ	B]�BZ�BezB\BU�BAUBDgBKBZ7BYKB=B+�B7LB!B��B�}B0B�B�B�B�ZB�B�BȚB�aB�UB�oB��B�eB��B|PB��B�KB��B��B��Bz�BshBe�B[�BUgBD�BSuB:�B/B7fB7�B/�BBBB'B
��B
�IB
�B
��B
�`B
��B
�B
�VB
x�B
\B
O�B
<jB
_B
1B
 B
�B
�B
�B	��B
[B	�rB	�B	�vB	�oB	�B	��B	׍B	�sB	�&B	�B	ϑB	�vB	�~B	B	�+B	�wB	�B	�JB	�WB	��B	�7B	�fB	��B	�%B	}<B	|�B	tB	g�B	_�B	T�B	N�B	AoB	>B	B[B	<jB	/�B	-wB	(XB	 �B	�B	'mB	�B	_B	�B	 �B��B	B	�B	 �B��B�-B�[B�AB�5B�mB�B�0B�7B��B��BĜBāB��B�)B��B��B�B�AB��B��B�B��B��B�xB�B�[B�B�2B�B�tBv�B��B��B��B��B|�Bu?Be�Bk�Bb�B_�Bl�BhXBbNB\]BR�BW?BOBBF�BR�BT�BO�BJ�BEB?.B@OBA�BH�B?}B:�B&�B#�B:DB7fB6FB5?B.cB5�B7�B+�B'B5�B4�B0�B1�B,�B(
B)�B+�B$�BmB@B�B�BqB�BgB�B�B�BvBNBB+B:BjB�B
XB�B�B	�BKB(B	RB�BB)BgBBB�B
BB
B�B1B$B�B�BB!�B&LB"�B vB'8B'RB+QB,qB'�B(�B&�B!-B!bB)�B5�B9�B8�B3�B/B/B9�B6B1[B:B@ B>B9XB1vBB'BAUBDgBB�BF�BI�BRoBSuBR�BQ�BU�BV�B]�B_�B_�Bc�Bg�Bf�Bk�Bj�Bi*BqBt�BtBr-Br-BsMBwLB{JB~BB�4B�UB�;B�;B�4B�OB}VB|�B{�B��B��B��B��B��B��B��B��B��B��B�4B�#B� B�B�$B�XB�CB�IB�cB�iB�vB��B��B��B��B��B��B��B�.B�B�B�B�6BɠB�vBөB�hB�B�B�B�B��B�RB�B�B�B�"B�6B�BB	'B	GB	9B	EB	YB	?B	SB	�B	�B	
�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%B	'B	(�B	)B	(
B	)B	*B	,"B	,=B	.IB	/5B	/5B	.IB	0oB	0�B	5tB	8�B	;�B	=�B	<�B	<�B	>�B	F�B	K�B	K�B	K�B	K�B	M�B	P�B	O�B	PB	QB	TB	UB	V9B	X+B	YeB	]dB	_pB	^jB	`vB	c�B	dtB	dtB	f�B	f�B	g�B	h�B	j�B	k�B	j�B	k�B	k�B	k�B	i�B	l�B	t�B	y	B	y	B	zB	{0B	�B	�'B	�UB	�aB	�YB	�_B	�XB	�xB	�xB	�^B	�~B	�jB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�,B	�
B	�B	�B	�6B	�=B	�5B	�;B	�AB	�GB	�[B	�hB	�ZB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�NB	�FB	�+B	�?B	�KB	�kB	�]B	�IB	�OB	�VB	�VB	ބB	�pB	ߊB	�bB	�B	�B	�tB	�zB	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�B	�B	�B
 4B
 B
;B
;B
 B
B
GB
'B
;B
[B
GB
GB
9B
EB
SB
gB
KB
	RB
xB
xB
dB
^B
xB
dB

rB
�B
pB
vB
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
 �B
 �B
"B
#�B
#�B
$B
"�B
"�B
!�B
$B
#B
$B
%,B
'B
(
B
&B
'B
)B
'B
'B
)DB
)*B
*KB
,"B
-)B
./B
./B
./B
/5B
/5B
/5B
/5B
0;B
0;B
0UB
0!B
0!B
0;B
0UB
1AB
1AB
2GB
33B
2GB
2GB
0UB
2aB
4nB
4TB
5ZB
5ZB
4nB
5ZB
5ZB
5ZB
5tB
6`B
6`B
7fB
7fB
8lB
9rB
9rB
9�B
:^B
:xB
:^B
9rB
8�B
8�B
9rB
9rB
8�B
6�B
5�B
3�B
>�B
?�B
>�B
=�B
=�B
?�B
?�B
A�B
B�B
A�B
@�B
A�B
A�B
B�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
F�B
F�B
F�B
F�B
E�B
F�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
I�B
K�B
K�B
K�B
MB
L�B
M�B
M�B
M�B
L�B
L�B
MB
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
RB
QB
O�B
Q B
Q B
R B
S&B
S&B
SB
R:B
SB
S�B
TB
TB
T�B
T�B
TB
T,B
S&B
U2B
U2B
V9B
VB
VB
UB
UB
U2B
VB
W?B
W$B
W$B
X+B
X+B
Y1B
X+B
X+B
Y1B
Y1B
Y1B
Y1B
X+B
Y1B
ZB
[#B
[=B
[#B
ZB
Z7B
YKB
X_B
ZQB
[=B
]/B
]/B
\]B
]IB
]IB
]IB
]IB
\CB
\]B
]IB
_;B
_;B
_;B
_VB
^jB
^OB
_VB
_VB
`BB
abB
`\B
aHB
`vB
`BB
_VB
^jB
`\B
bNB
bNB
a|B
abB
abB
`vB
abB
cTB
dZB
dZB
cnB
cnB
e�B
dtB
ezB
ffB
e�B
ezB
ffB
f�B
ezB
e�B
ffB
ffB
f�B
ffB
g�B
gmB
gmB
f�B
f�B
g�B
g�B
g�B
h�B
iyB
i�B
jB
iyB
i�B
i�B
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
l�B
l�B
l�B
k�B
k�B
k�B
j�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
p�B
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
t�B
s�B
s�B
r�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
xB
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<0�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807260034552018072600345520180726003455201807260200162018072602001620180726020016201807270024042018072700240420180727002404  JA  ARFMdecpA19c                                                                20180724093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180724004211  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180724004214  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180724004214  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180724004215  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180724004215  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180724004215  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180724004215  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180724004216                      G�O�G�O�G�O�                JA  ARUP                                                                        20180724005605                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180722153407  CV  JULD            G�O�G�O�FÝ�                JM  ARCAJMQC2.0                                                                 20180725153455  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180725153455  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180725170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180726152404  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                