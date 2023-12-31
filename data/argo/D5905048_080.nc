CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-01-19T00:35:40Z creation;2017-01-19T00:35:42Z conversion to V3.1;2019-12-19T08:17:02Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170119003540  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               PA   JA  I2_0577_080                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��r��S�1   @��sM���@3�=�b��d�`A�7L1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  D   D � D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D��3D�3D�C3D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@7
=@}p�@��R@��RA\)A=A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C\C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�)C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Cb\Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���D }qD �qD}qD�D��D�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\��D\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D���D��D�A�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ĜAμjA�A���A�ƨA���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���AΣ�AΕ�A΃A΃A΍PAΧ�A���A��mAΑhA�^5A�5?A˩�A��/AƁA�`BA�A��/A¶FA�hsA�1A�ƨA���A�9XA���A��9A���A�n�A��hA���A��uA��A�S�A���A��HA�?}A���A��A�(�A��#A�n�A�7LA���A�ȴA�^5A��9A���A�  A���A�bNA�(�A�ffA��TA��;A�ZA��;A�n�A�ZA��RA���A�-A��A���A��A�{A��DA��A�?}A��A�$�A�oA���A��`A�;dA�$�A��
A�t�A�/A���A�?}A���A�XA��9A�\)A��HA��A��FA��-A��DA���A�l�A�A�A���A��;A�ffA�ƨA33A}%Az�/Ay�-Awp�At��Ar�`ApZAm�#Ajz�Af��Adn�Ac�wAc\)Aa|�A`~�A]��A[+AX�9AV��AU�PAR=qAP-AO?}AM33AIhsAE��ADffACVAB-AA�;AAhsA@�yA@M�A<ffA;O�A:=qA9A6ZA4v�A3�PA333A2��A2A�A1�wA/;dA-��A-+A,�9A,M�A+K�A*JA)ƨA)��A*A�A*Q�A)A(��A($�A'33A&ZA%�mA%��A$��A#��A#%A!�
A ��AƨA"�A��A��A/A�jA�+A5?AE�Ax�A�A(�A�RAVA{A��A
=Ar�AXAr�A�A�A�A
Q�A	XA^5A��A33A|�A��A�RA~�AAAO�A �!@���@���@��@�O�@��/@���@��@�@��@�1@�@�+@��@�@��@�j@��m@@�\)@��y@��@��@�D@�(�@�w@�l�@��H@�V@���@�Z@�dZ@��@��@�j@�S�@��/@�\)@��H@�{@�I�@�bN@�j@�bN@�I�@�n�@�X@��#@և+@�33@���@�ƨ@�-@�G�@��y@Ѳ-@�/@��`@Гu@϶F@υ@�dZ@��@��@̬@�  @�E�@���@�@�%@Ƨ�@�bN@��@��@�X@�/@��j@�\)@���@�I�@�  @�ƨ@�K�@�n�@��T@���@�@��-@��h@�7L@��/@�Z@���@���@�/@���@�j@�b@��
@�ƨ@��F@��@�dZ@��@���@��^@��@��D@� �@�t�@�o@��y@��R@�v�@�{@��^@���@�7L@���@�1'@��@�7L@�O�@�V@���@��@�33@�^5@��@�33@�
=@���@��#@��@���@���@�n�@��\@��\@���@��@��y@��@�ȴ@�{@�J@��-@� �@�t�@��@�ƨ@���@�t�@���@�ff@�^5@�J@��#@�@�@�@�l�@�7L@���@�x�@��@���@��@��@�-@�7L@�9X@��w@���@�l�@�K�@�
=@�@���@���@��^@��7@�hs@���@��9@��j@��@��h@�{@�33@�dZ@�+@�"�@��y@���@�v�@�V@�5?@���@���@��-@���@�X@�V@�ȴ@���@�b@�S�@�o@��@�~�@�$�@�@�hs@�O�@��h@�O�@���@��9@��@�bN@�Z@�b@�(�@�V@�G�@�V@�bN@�33@�33@�dZ@�|�@�|�@��@�\)@���@���@���@��!@��\@���@���@��!@���@�-@��#@��@�O�@�7L@�r�@�9X@�1@�ƨ@��@��@�@�v�@�$�@�@�x�@��@���@��u@�j@�Q�@�I�@�I�@�1@� �@�9X@��@�o@��@�ȴ@��!@��+@�=q@��@��#@�x�@�?}@�/@��@��9@�Z@�(�@�1@�ƨ@���@��P@�;d@�
=@�@���@��+@�$�@��@���@���@�x�@�O�@�&�@�Ĝ@�bN@��m@��P@�t�@�S�@�+@�ȴ@�n�@��@�x�@��@��j@���@�r�@�A�@��@�w@\)@+@~ȴ@~��@~v�@}��@|�/@|�D@{��@{�
@{�
@{�F@{C�@z��@z�!@y��@y�@x�`@x�@xbN@xQ�@xQ�@xA�@x1'@x �@x  @w�;@w�@w��@w�P@v�y@v$�@u?}@uV@t��@t�/@t��@t�@t�@sC�@r�@rn�@q�^@qX@p��@p��@pr�@pbN@pA�@pb@o�@o;d@o+@nȴ@nV@m��@m/@l�/@l�@lz�@lZ@l(�@k��@j�\@jM�@j=q@j-@j�@i��@iX@i7L@i�@h��@h��@hQ�@g�P@f��@f��@f�+@fv�@f5?@e�h@eO�@e?}@e�@d�@d��@d�@d�D@d(�@c�@b��@bM�@ahs@`��@`�`@`��@`bN@` �@`b@_�P@_+@^�R@^��@^�+@^ff@^ff@^V@^$�@]`B@\�/@\�j@\�D@\I�@\(�@\1@[ƨ@Z�H@Z-@Y��@Y�7@Yx�@YX@X�`@X�@X�@X�@XbN@W��@W\)@W�@Vȴ@Vff@U�-@UO�@U�@T�/@Tz�@T�@S�
@St�@R��@RJ@P��@P��@PbN@Pr�@PQ�@P �@O�;@O��@O��@O\)@O+@N�y@Nȴ@N�+@N@M�@L��@L��@Lj@Kƨ@KC�@K"�@K@Jn�@I�#@I��@IX@I&�@I%@H�9@Hr�@HA�@H �@H  @G�@G�P@Gl�@G�@F��@Fv�@F$�@E��@EO�@EO�@EV@D��@DZ@D(�@D1@C�
@Ct�@CC�@C"�@B�H@B^5@A��@A��@AX@A&�@@��@@bN@@ �@@ �@@ �@@  @?�@?�@?�@?\)@?;d@>�R@=@=p�@<��@<9X@;�
@;��@;�@;dZ@;S�@;C�@;o@;@:�@:��@:��@:^5@:�@9��@97L@8��@8��@8r�@8  @7�P@7+@6�@6@5�-@5�h@5O�@5�@4�@4�@3�F@3t�@3dZ@3S�@333@3"�@3o@2��@2~�@2-@1��@1�#@1��@1hs@1&�@0�9@0�u@0Q�@01'@/�@/�@/|�@/K�@/
=@.�R@.@-�-@-p�@,��@,�@,j@+��@+�
@+�F@+�F@+��@+��@+t�@+S�@*�@*��@*~�@*M�@)��@)�#@)�^@)�7@)hs@)�@(�`@(��@(r�@( �@(  @'�@'��@'��@'|�@';d@'
=@&��@&{@%��@$�j@$z�@$j@$�@#�m@#��@#��@#��@#"�@"�!@"�\@"M�@"J@!��@!��@!�@!�@!��@!hs@!&�@!%@ �9@ r�@ Q�@�;@�@+@�@��@ȴ@E�@{@@�@@��@��@�@p�@`B@/@�D@z�@j@9X@�F@t�@dZ@o@��@��@n�@��@x�@x�@��@�9@�u@�@r�@Q�@b@�;@�@|�@|�@l�@\)@K�@+@+@�@�y@��@�+@ff@V@ff@V@E�@E�@E�@5?@@@�-@p�@`B@`B@?}@/@/@V@��@�/@�j@��@z�@I�@�@�
@��@��@t�@C�@33@"�@o@o@o@o@@�@�H@��@~�@^5@M�@=q@=q@�@�@�#@��@��@��@x�@X@7L@7L@�@�`@�9@�@Q�@1'@ �@  @�;@�@�P@l�@K�@;d@;d@+@�@�@v�@V@{@��@`B@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ĜAμjA�A���A�ƨA���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���AΣ�AΕ�A΃A΃A΍PAΧ�A���A��mAΑhA�^5A�5?A˩�A��/AƁA�`BA�A��/A¶FA�hsA�1A�ƨA���A�9XA���A��9A���A�n�A��hA���A��uA��A�S�A���A��HA�?}A���A��A�(�A��#A�n�A�7LA���A�ȴA�^5A��9A���A�  A���A�bNA�(�A�ffA��TA��;A�ZA��;A�n�A�ZA��RA���A�-A��A���A��A�{A��DA��A�?}A��A�$�A�oA���A��`A�;dA�$�A��
A�t�A�/A���A�?}A���A�XA��9A�\)A��HA��A��FA��-A��DA���A�l�A�A�A���A��;A�ffA�ƨA33A}%Az�/Ay�-Awp�At��Ar�`ApZAm�#Ajz�Af��Adn�Ac�wAc\)Aa|�A`~�A]��A[+AX�9AV��AU�PAR=qAP-AO?}AM33AIhsAE��ADffACVAB-AA�;AAhsA@�yA@M�A<ffA;O�A:=qA9A6ZA4v�A3�PA333A2��A2A�A1�wA/;dA-��A-+A,�9A,M�A+K�A*JA)ƨA)��A*A�A*Q�A)A(��A($�A'33A&ZA%�mA%��A$��A#��A#%A!�
A ��AƨA"�A��A��A/A�jA�+A5?AE�Ax�A�A(�A�RAVA{A��A
=Ar�AXAr�A�A�A�A
Q�A	XA^5A��A33A|�A��A�RA~�AAAO�A �!@���@���@��@�O�@��/@���@��@�@��@�1@�@�+@��@�@��@�j@��m@@�\)@��y@��@��@�D@�(�@�w@�l�@��H@�V@���@�Z@�dZ@��@��@�j@�S�@��/@�\)@��H@�{@�I�@�bN@�j@�bN@�I�@�n�@�X@��#@և+@�33@���@�ƨ@�-@�G�@��y@Ѳ-@�/@��`@Гu@϶F@υ@�dZ@��@��@̬@�  @�E�@���@�@�%@Ƨ�@�bN@��@��@�X@�/@��j@�\)@���@�I�@�  @�ƨ@�K�@�n�@��T@���@�@��-@��h@�7L@��/@�Z@���@���@�/@���@�j@�b@��
@�ƨ@��F@��@�dZ@��@���@��^@��@��D@� �@�t�@�o@��y@��R@�v�@�{@��^@���@�7L@���@�1'@��@�7L@�O�@�V@���@��@�33@�^5@��@�33@�
=@���@��#@��@���@���@�n�@��\@��\@���@��@��y@��@�ȴ@�{@�J@��-@� �@�t�@��@�ƨ@���@�t�@���@�ff@�^5@�J@��#@�@�@�@�l�@�7L@���@�x�@��@���@��@��@�-@�7L@�9X@��w@���@�l�@�K�@�
=@�@���@���@��^@��7@�hs@���@��9@��j@��@��h@�{@�33@�dZ@�+@�"�@��y@���@�v�@�V@�5?@���@���@��-@���@�X@�V@�ȴ@���@�b@�S�@�o@��@�~�@�$�@�@�hs@�O�@��h@�O�@���@��9@��@�bN@�Z@�b@�(�@�V@�G�@�V@�bN@�33@�33@�dZ@�|�@�|�@��@�\)@���@���@���@��!@��\@���@���@��!@���@�-@��#@��@�O�@�7L@�r�@�9X@�1@�ƨ@��@��@�@�v�@�$�@�@�x�@��@���@��u@�j@�Q�@�I�@�I�@�1@� �@�9X@��@�o@��@�ȴ@��!@��+@�=q@��@��#@�x�@�?}@�/@��@��9@�Z@�(�@�1@�ƨ@���@��P@�;d@�
=@�@���@��+@�$�@��@���@���@�x�@�O�@�&�@�Ĝ@�bN@��m@��P@�t�@�S�@�+@�ȴ@�n�@��@�x�@��@��j@���@�r�@�A�@��@�w@\)@+@~ȴ@~��@~v�@}��@|�/@|�D@{��@{�
@{�
@{�F@{C�@z��@z�!@y��@y�@x�`@x�@xbN@xQ�@xQ�@xA�@x1'@x �@x  @w�;@w�@w��@w�P@v�y@v$�@u?}@uV@t��@t�/@t��@t�@t�@sC�@r�@rn�@q�^@qX@p��@p��@pr�@pbN@pA�@pb@o�@o;d@o+@nȴ@nV@m��@m/@l�/@l�@lz�@lZ@l(�@k��@j�\@jM�@j=q@j-@j�@i��@iX@i7L@i�@h��@h��@hQ�@g�P@f��@f��@f�+@fv�@f5?@e�h@eO�@e?}@e�@d�@d��@d�@d�D@d(�@c�@b��@bM�@ahs@`��@`�`@`��@`bN@` �@`b@_�P@_+@^�R@^��@^�+@^ff@^ff@^V@^$�@]`B@\�/@\�j@\�D@\I�@\(�@\1@[ƨ@Z�H@Z-@Y��@Y�7@Yx�@YX@X�`@X�@X�@X�@XbN@W��@W\)@W�@Vȴ@Vff@U�-@UO�@U�@T�/@Tz�@T�@S�
@St�@R��@RJ@P��@P��@PbN@Pr�@PQ�@P �@O�;@O��@O��@O\)@O+@N�y@Nȴ@N�+@N@M�@L��@L��@Lj@Kƨ@KC�@K"�@K@Jn�@I�#@I��@IX@I&�@I%@H�9@Hr�@HA�@H �@H  @G�@G�P@Gl�@G�@F��@Fv�@F$�@E��@EO�@EO�@EV@D��@DZ@D(�@D1@C�
@Ct�@CC�@C"�@B�H@B^5@A��@A��@AX@A&�@@��@@bN@@ �@@ �@@ �@@  @?�@?�@?�@?\)@?;d@>�R@=@=p�@<��@<9X@;�
@;��@;�@;dZ@;S�@;C�@;o@;@:�@:��@:��@:^5@:�@9��@97L@8��@8��@8r�@8  @7�P@7+@6�@6@5�-@5�h@5O�@5�@4�@4�@3�F@3t�@3dZ@3S�@333@3"�@3o@2��@2~�@2-@1��@1�#@1��@1hs@1&�@0�9@0�u@0Q�@01'@/�@/�@/|�@/K�@/
=@.�R@.@-�-@-p�@,��@,�@,j@+��@+�
@+�F@+�F@+��@+��@+t�@+S�@*�@*��@*~�@*M�@)��@)�#@)�^@)�7@)hs@)�@(�`@(��@(r�@( �@(  @'�@'��@'��@'|�@';d@'
=@&��@&{@%��@$�j@$z�@$j@$�@#�m@#��@#��@#��@#"�@"�!@"�\@"M�@"J@!��@!��@!�@!�@!��@!hs@!&�@!%@ �9@ r�@ Q�@�;@�@+@�@��@ȴ@E�@{@@�@@��@��@�@p�@`B@/@�D@z�@j@9X@�F@t�@dZ@o@��@��@n�@��@x�@x�@��@�9@�u@�@r�@Q�@b@�;@�@|�@|�@l�@\)@K�@+@+@�@�y@��@�+@ff@V@ff@V@E�@E�@E�@5?@@@�-@p�@`B@`B@?}@/@/@V@��@�/@�j@��@z�@I�@�@�
@��@��@t�@C�@33@"�@o@o@o@o@@�@�H@��@~�@^5@M�@=q@=q@�@�@�#@��@��@��@x�@X@7L@7L@�@�`@�9@�@Q�@1'@ �@  @�;@�@�P@l�@K�@;d@;d@+@�@�@v�@V@{@��@`B@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BDBDBJBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBVB�B�B�B+B1'BD�BiyB�oB�wB�BbB(�B-B7LB49B,B �B�B�B�B�B�B�B�B$�BA�BF�B[#Bu�BjB^5BYBW
BW
B[#Be`BiyBm�Bp�Bs�Br�B�=B�PB�JB�%Bu�Bm�Bn�Bv�B�DB��B��B��B��B�bB�B�B|�By�Bo�BaHBN�BE�B>wB.B(�B�BhB��B�B�B�ZB�B��B�wB��B��B�bB�DB�%B� Bm�BXBA�B5?B/B&�B�B+B
��B
�ZB
��B
ŢB
�3B
�B
��B
�\B
�B
p�B
^5B
L�B
D�B
7LB
$�B
oB
%B	�B	�HB	��B	�}B	�XB	�XB	�B	��B	�VB	� B	q�B	cTB	_;B	K�B	>wB	6FB	,B	�B	%B	  B��B��B�B�B�B�yB�/B��B��BɺBĜB�wB�jB�^B�^B�XB�RB�jB�^B�dB�jB�wBƨB�B�HB�fB�B��B��B	  B��B��B��B	  B��B��B��B��B��B	B��B	B��B��B��B�B�B�B�B�B�B�ZB�BB�#B��B��B��B��B��BȴBǮBŢBŢBŢBĜBŢBƨBŢBȴBǮBǮBƨBǮBƨBĜBŢBÖBÖBŢBǮBǮB��B��BȴBŢBŢBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴBÖB�}B�qB�wB�jB�dB�^B�dB�dB��BƨBɺB��B��B�B�HB�`B�yB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B��B��B	B		7B	DB	PB	VB	PB	\B	uB	{B	�B	�B	�B	�B	 �B	 �B	!�B	!�B	$�B	'�B	,B	.B	2-B	9XB	=qB	?}B	A�B	A�B	A�B	B�B	B�B	C�B	D�B	F�B	K�B	K�B	L�B	K�B	J�B	J�B	I�B	I�B	I�B	H�B	H�B	I�B	L�B	P�B	S�B	VB	bNB	cTB	e`B	ffB	gmB	gmB	ffB	l�B	o�B	r�B	t�B	u�B	p�B	n�B	l�B	q�B	t�B	u�B	u�B	y�B	{�B	|�B	{�B	{�B	~�B	�B	~�B	}�B	�B	�B	�B	�%B	�1B	�1B	�+B	�1B	�1B	�7B	�DB	�uB	��B	��B	�B	�B	�!B	�!B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	��B	��B	B	ÖB	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�;B	�)B	�;B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B

=B

=B
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
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
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
/B
/B
0!B
/B
/B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
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
;dB
;dB
;dB
:^B
;dB
:^B
:^B
;dB
=qB
=qB
>wB
>wB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
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
e`B
e`B
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
ffB
ffB
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
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
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
t�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BDBDB0BDBDBDBDBDBDBDBDBDB^BDBDBDBDBDBpB�B�B�B+B1[BD�Bi_B�:B�]B�B�B+�B/�B9�B9�B0oB#TB!bB�BBWB�B�B�B($BC-BH�B]�BxBl�Ba�B[�BX�BX_B\�BgBj�BoBraBu�BuZB��B�"B��B�KBx�Bn�Bo5BxB�B�B�eB�7B��B�TB��B�[B~�B|�BshBe,BPbBHKBAUB/�B+�B"4B{B��B�MB��B��B��B͹B��B�B�sB�NB�0B��B��Bp�B[=BC-B6`B0�B)�B�B	�B
�B
��B
��B
�B
��B
��B
��B
��B
��B
shB
`�B
N�B
GzB
:DB
'�B
�B
	�B	��B	�FB	�sB	��B	�xB	��B	�B	�8B	��B	��B	t9B	e�B	b�B	N<B	@iB	9�B	0�B	�B	�B	�B��B�ZB�B�B�}B�]B��B՛B�B��B��B��B�"B�JB�0B��B�0B�B�JB�B�VB��B��B�EB�B�fB��B��B	 B	 B�(B��B��B	 �B�B�cB�PB��B�qB	[B�BB	�B	 4B��B�rB�MB�B��B��B�iB�MB�fB�4B�]BյB�B��B�hB��B�RB�1BƎB�EB��B��BƨB��BǮB�RB�KB�KB��B�#B�zBŢB��B�gB�B��B�1BȴB�PB�xBɆB�?B�?B�YB�KB�B�PB�pB�BB�<B�(B�\BѷBөB�[B�@B�uB�[BӏB��B�,B�gB̳BɺB�MB�B��B� B�VB�B�B�jB�B��B��B�#B��B�\B��B��B�B�_B�B�B�B�JB��B�8B�B�%B�ZB�B�"B�wB��B��B��B��B�dB	gB	B	�B��B�}B	�B		lB	�B	�B	\B	pB	.B	�B	�B	�B	+B	�B	�B	 �B	 �B	!�B	"B	%,B	(sB	,�B	.�B	33B	9�B	=�B	?�B	A�B	A�B	A�B	B�B	B�B	C�B	EB	GEB	LdB	L0B	M6B	LJB	K)B	J�B	I�B	I�B	J#B	IB	H�B	J#B	M6B	QB	S�B	U�B	b�B	c�B	e�B	f�B	hXB	g�B	fLB	lqB	o�B	s3B	utB	v�B	q[B	o5B	l�B	q�B	t�B	u�B	u�B	y�B	|B	}"B	|PB	|B	}B	�B	cB	}�B	�B	�'B	�gB	��B	��B	�KB	�_B	�KB	�1B	�B	�B	��B	��B	��B	�IB	�IB	��B	��B	�B	�B	��B	�zB	�B	��B	��B	��B	�B	�OB	��B	��B	��B	� B	�B	�DB	�B	� B	��B	��B	��B	� B	��B	�B	��B	�(B	�(B	�B	��B	�B	�B	�B	�,B	�,B	�FB	ԯB	ӏB	��B	�B	�'B	��B	��B	��B	��B	�B	��B	��B	ɺB	�B	�B	��B	�B	��B	��B	��B	��B	�}B	�1B	�xB	��B	߾B	�)B	�!B	�fB	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	��B	�%B	�%B	��B	��B	��B	�9B	��B	��B	�	B	�	B	�$B	�8B	�2B	�B	�B	�B	�>B	�$B	�*B	��B	��B	��B	�B	�B	��B	�(B
uB
 OB
 4B
 4B
 4B
 4B
;B
;B
;B
[B
MB
9B
SB
mB
YB
_B
KB
fB

XB

rB
xB
dB
~B
~B
�B
�B
�B
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
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
 �B
!-B
"B
# B
#B
#B
"�B
"�B
#B
# B
$&B
$B
%B
%,B
&B
%�B
%�B
%�B
&�B
%�B
'B
'8B
'B
'B
'B
($B
($B
($B
)B
)B
)B
)B
)B
)DB
*eB
+B
+B
+B
+6B
+6B
+B
+B
+B
,"B
+6B
+6B
+QB
,=B
,B
,"B
,"B
,"B
,=B
-CB
-CB
-)B
.IB
.IB
./B
./B
.IB
.cB
/iB
0UB
0�B
0UB
/5B
/OB
0UB
/5B
/5B
0UB
1AB
0UB
1AB
1AB
1AB
1AB
2aB
3hB
3�B
4�B
5ZB
5tB
5tB
5tB
6`B
6�B
6�B
7�B
7�B
8lB
8lB
8lB
8�B
8lB
8lB
8RB
8lB
8�B
9�B
9rB
9�B
9�B
9�B
:xB
:xB
:xB
:�B
:�B
;�B
;�B
;�B
:�B
;�B
:xB
:xB
;dB
=�B
=�B
>�B
>wB
=�B
=�B
=�B
=�B
>�B
>�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
K�B
MB
MB
N"B
N<B
N�B
OB
OB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
QB
QB
Q B
QB
R B
R B
S@B
S&B
S[B
TB
TB
TB
T,B
UB
U2B
UMB
VB
VB
VB
VB
VB
V9B
W$B
W?B
W$B
X+B
X+B
XEB
XEB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
ZQB
Z7B
[WB
[WB
[WB
\]B
\CB
\]B
\CB
\]B
]IB
]IB
]IB
^5B
^5B
^5B
^OB
^OB
^OB
^OB
^jB
^OB
^OB
_VB
_pB
_VB
_VB
_VB
_pB
_pB
`\B
`\B
`\B
`BB
`vB
`\B
`\B
`\B
abB
a|B
a|B
b�B
b�B
cnB
cnB
c�B
cnB
cnB
dZB
dtB
d�B
d�B
ezB
ezB
ezB
e`B
e`B
e`B
ezB
e�B
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
hsB
h�B
hsB
h�B
h�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
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
t�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y�B
zB
zB
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201701230033372017012300333720170123003337201806221308032018062213080320180622130803201804050708392018040507083920180405070839  JA  ARFMdecpA19c                                                                20170119093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170119003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170119003541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170119003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170119003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170119003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170119003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170119003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170119003542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170119003542                      G�O�G�O�G�O�                JA  ARUP                                                                        20170119010344                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170119153649  CV  JULD            G�O�G�O�F�S�                JM  ARCAJMQC2.0                                                                 20170122153337  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170122153337  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220839  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040803  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                