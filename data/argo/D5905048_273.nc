CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-21T00:35:22Z creation;2018-08-21T00:35:27Z conversion to V3.1;2019-12-19T07:30:43Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180821003522  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_273                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�{4:7_ 1   @�{5+�d�@4b���m�db���S�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D��3D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��RA\)A?\)A`��A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�
D/}qD/�qD0w
D0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDb�Db}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�D���D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܻ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D��D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A�JA��A�A���A�\)A�G�A�5?A�+A��A�bA�A���A���A��yA���Aܛ�A�x�A�^5A�I�A�O�AܸRAܩ�A܍PA܅A܇+A܋DA܃A�hsA۶FA��A��yA�-AԾwAӋDA��A���A҉7A�l�A�JAэPA���A��A�r�A��A��AήA�7LA�$�A��A�ƨA͛�A���A̼jA̴9A�A�Q�A�M�Aȟ�A�VA�|�A��TA�$�AŸRA�l�A���A�O�A¾wA�A�z�A��FA�n�A�Q�A�"�A��A��!A��A�=qA��RA�Q�A�7LA�Q�A�
=A�bA���A�G�A�A�A���A��A�|�A�x�A�/A���A���A�G�A���A�hsA��A���A�G�A�/A�
=A�S�A��A���A��A�VA��A���A���A��yA��jA��^A��TA�5?A��A�hsA�K�A��RA�(�A�K�A�
=A�G�A���A��wA�G�A�bA|z�AwƨAr�An��Al{Ah��AaS�A_�A^�\A\~�AZ��AZ-AV��AS��AR{APz�API�AOXAMAL�DAK�-AI�
AI
=AG��AF��AE�ACS�AB��A@��A>��A=p�A<ĜA:r�A9`BA9VA8��A77LA5VA3p�A2�+A1|�A0�uA/�A/G�A.n�A+
=A'�A'oA$��A$VA#�^A#|�A"�A ��Al�AE�A��Ap�AO�A
=A�+A-A&�A�A?}A�A�!A��A�FA�hA��AhsA��A  A
A�A	��A	ƨA	"�AA��Ap�A��A�!A^5A�7AO�A"�A�yAVAƨA?}@��@�@��j@��@���@�ȴ@��#@���@�C�@��@�Z@�@�&�@��@�r�@��H@�-@�u@�l�@�X@�t�@�@�ȴ@ݲ-@�Ĝ@�r�@��m@�K�@ڧ�@ّh@׶F@�x�@�r�@�1@�t�@ҟ�@�ff@Ѻ^@Гu@ϕ�@�o@Ο�@�E�@�`B@���@̓u@��H@���@�r�@ƸR@��@���@���@�?}@�Z@�ƨ@�;d@�@��H@�@�ff@��@�p�@���@��
@��@��!@��7@�V@��D@�z�@�j@��w@�33@��!@�G�@���@�z�@��F@�K�@��@�^5@��h@�V@�Z@��P@��@�ȴ@�=q@��@�z�@�Q�@��@�7L@�1@��y@��H@�n�@�E�@��@�A�@�  @��m@��
@��w@�|�@�+@��@���@�M�@���@�7L@���@��/@���@��u@�j@��@��@�K�@�"�@���@�ff@�5?@�@���@��h@�`B@���@��9@�bN@�Q�@�A�@�(�@�b@��
@���@�|�@�\)@�S�@�S�@�S�@�S�@�K�@�33@���@��\@�n�@�@��#@��^@��h@�`B@���@�r�@�A�@��;@�l�@�;d@���@��+@�ff@�$�@�@��h@�hs@��`@��@��@�bN@� �@��m@�ƨ@�ƨ@���@��@�|�@�\)@�;d@��@��@��H@���@�~�@�=q@���@��^@�hs@�X@���@�9X@���@��
@��@���@���@�|�@��@�M�@���@��-@��7@�`B@�G�@���@�Ĝ@��D@�Z@�(�@��@�1@�1@��m@�t�@�\)@�C�@��@�v�@�{@���@��h@�O�@���@� �@��
@���@�+@���@��@��!@�M�@��@�J@��@�x�@���@��u@�j@�j@�Z@�Z@�1'@�  @���@��P@�C�@�ȴ@�~�@�E�@�$�@��@��h@�`B@�?}@��@��@���@�r�@�  @�ƨ@��w@���@�K�@���@�^5@�V@�M�@�E�@�$�@�@��T@��-@�?}@��`@���@��@�Q�@��u@��@�r�@�I�@�1@��P@�dZ@�;d@�;d@�+@�@��@���@���@���@�5?@�@���@��@��#@��h@�X@�&�@��`@��9@���@��u@�bN@�9X@�b@�b@�1@�  @�w@\)@~ȴ@~ff@}�@}�T@}�@}��@}�h@}/@|�/@|z�@|1@{C�@z��@z-@y��@yX@y%@xĜ@x�u@x �@w�w@v�R@v�+@vE�@v$�@u�@u/@tj@t1@st�@s33@s"�@r�H@r�\@rM�@r^5@r�\@rM�@q�^@qhs@qX@qhs@r�@q�@q�^@qhs@p��@pbN@o�;@o�P@o+@nȴ@m��@m/@l�/@l�@l�D@lz�@lj@l�@l1@l1@k�m@k�
@kƨ@k�F@k��@k��@k��@kdZ@kS�@kC�@j��@j^5@j-@iG�@i%@hĜ@h �@g��@g;d@g;d@g�@f�+@e�T@e/@d9X@d(�@d�@c��@c�
@c�F@c�@cdZ@cC�@b�H@b^5@a��@a�7@aG�@`��@` �@_��@_K�@_+@^ȴ@^5?@]��@]`B@]�@\�@\�D@\�@[�m@[��@[t�@Z��@Z~�@Y��@Yx�@X��@X��@X  @W\)@W+@W
=@V�R@VE�@V$�@V{@U��@Up�@U`B@U`B@U`B@UV@T�j@Tj@T9X@T1@St�@R�!@Q�#@Qx�@QX@QG�@Q7L@Q&�@Q&�@P��@P��@PA�@O�@O��@O�@O\)@O�@N��@N�+@NE�@M�@M@M�h@Mp�@L�@L�@K��@K�@K�@K33@J�H@Ko@K"�@J��@J~�@Jn�@I7L@H�9@Hr�@HA�@G��@G�P@G\)@G+@G
=@F�y@Fv�@F{@Ep�@E?}@D��@DI�@C��@C��@CS�@C@B�!@B^5@B�@B�@Bn�@B�!@Bn�@B=q@A�#@A��@A&�@@�`@@r�@@b@?�;@?�w@?�@?�P@?K�@>��@>�+@=��@=`B@=/@<�@<�@<Z@;��@;�
@;ƨ@;o@:�@:�!@:�!@:^5@:�@9�^@9G�@9�@8��@8�`@8�u@8�@8Q�@8 �@8b@8  @7�@7��@7�@6�y@6ȴ@6V@5��@5V@4��@4�j@4j@3��@3��@3"�@2�!@2n�@2^5@2�@1�^@1x�@1�@0�9@0�@01'@/�@/�P@.��@.�R@.��@.��@.��@.��@.�+@.V@.@-?}@,�j@,��@,z�@,Z@,(�@+�m@+��@+S�@*�H@*n�@*=q@)��@)�#@)��@)��@)��@)�7@)&�@(�9@(�@(Q�@( �@(b@(b@'�;@'�@'�P@'K�@'�@&�@&ȴ@&��@&��@&v�@&V@&E�@&5?@&$�@%�@%@%�h@%p�@%`B@%?}@$��@$�@$��@$I�@$(�@$�@#��@#�m@#��@#�@#"�@"��@"^5@"-@!��@!��@!X@!G�@!�@ �`@ �9@ Q�@ A�@ 1'@  �@  �@ b@   @�@�w@|�@l�@�@��@v�@5?@$�@{@��@�h@�@p�@p�@p�@`B@/@��@�/@�@j@I�@9X@�@�m@dZ@S�@S�@S�@S�@C�@C�@C�@33@o@�@^5@�@J@��@�#@��@�7@7L@%@�9@�@bN@Q�@ �@  @�@�w@|�@l�@\)@K�@;d@
=@�@�R@V@@@��@�@/@V@�@�j@9X@�m@�F@dZ@@�@�H@��@��@�\@n�@M�@-@�@��@��@�7@X@G�@�@�9@�u@r�@1'@��@��@|�@;d@�@�y@ȴ@��@V@E�@5?@{@�@�-@O�@��@�@z�@9X@9X@�m@��@�@�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A�JA��A�A���A�\)A�G�A�5?A�+A��A�bA�A���A���A��yA���Aܛ�A�x�A�^5A�I�A�O�AܸRAܩ�A܍PA܅A܇+A܋DA܃A�hsA۶FA��A��yA�-AԾwAӋDA��A���A҉7A�l�A�JAэPA���A��A�r�A��A��AήA�7LA�$�A��A�ƨA͛�A���A̼jA̴9A�A�Q�A�M�Aȟ�A�VA�|�A��TA�$�AŸRA�l�A���A�O�A¾wA�A�z�A��FA�n�A�Q�A�"�A��A��!A��A�=qA��RA�Q�A�7LA�Q�A�
=A�bA���A�G�A�A�A���A��A�|�A�x�A�/A���A���A�G�A���A�hsA��A���A�G�G�O�G�O�A�S�A��A���A��A�VA��A���A���A��yA��jA��^A��TA�5?A��A�hsA�K�A��RA�(�A�K�A�
=A�G�A���A��wA�G�A�bA|z�AwƨAr�An��Al{Ah��AaS�A_�A^�\A\~�AZ��AZ-AV��AS��AR{APz�API�AOXAMAL�DAK�-AI�
AI
=AG��AF��AE�ACS�AB��A@��A>��A=p�A<ĜA:r�A9`BA9VA8��A77LA5VA3p�A2�+A1|�A0�uA/�A/G�A.n�A+
=A'�A'oA$��A$VA#�^A#|�A"�A ��Al�AE�A��Ap�AO�A
=A�+A-A&�A�A?}A�A�!A��A�FA�hA��AhsA��A  A
A�A	��A	ƨA	"�AA��Ap�A��A�!A^5A�7AO�A"�A�yAVAƨA?}@��@�@��j@��@���@�ȴ@��#@���@�C�@��@�Z@�@�&�@��@�r�@��H@�-@�u@�l�@�X@�t�@�@�ȴ@ݲ-@�Ĝ@�r�@��m@�K�@ڧ�@ّh@׶F@�x�@�r�@�1@�t�@ҟ�@�ff@Ѻ^@Гu@ϕ�@�o@Ο�@�E�@�`B@���@̓u@��H@���@�r�@ƸR@��@���@���@�?}@�Z@�ƨ@�;d@�@��H@�@�ff@��@�p�@���@��
@��@��!@��7@�V@��D@�z�@�j@��w@�33@��!@�G�@���@�z�@��F@�K�@��@�^5@��h@�V@�Z@��P@��@�ȴ@�=q@��@�z�@�Q�@��@�7L@�1@��y@��H@�n�@�E�@��@�A�@�  @��m@��
@��w@�|�@�+@��@���@�M�@���@�7L@���@��/@���@��u@�j@��@��@�K�@�"�@���@�ff@�5?@�@���@��h@�`B@���@��9@�bN@�Q�@�A�@�(�@�b@��
@���@�|�@�\)@�S�@�S�@�S�@�S�@�K�@�33@���@��\@�n�@�@��#@��^@��h@�`B@���@�r�@�A�@��;@�l�@�;d@���@��+@�ff@�$�@�@��h@�hs@��`@��@��@�bN@� �@��m@�ƨ@�ƨ@���@��@�|�@�\)@�;d@��@��@��H@���@�~�@�=q@���@��^@�hs@�X@���@�9X@���@��
@��@���@���@�|�@��@�M�@���@��-@��7@�`B@�G�@���@�Ĝ@��D@�Z@�(�@��@�1@�1@��m@�t�@�\)@�C�@��@�v�@�{@���@��h@�O�@���@� �@��
@���@�+@���@��@��!@�M�@��@�J@��@�x�@���@��u@�j@�j@�Z@�Z@�1'@�  @���@��P@�C�@�ȴ@�~�@�E�@�$�@��@��h@�`B@�?}@��@��@���@�r�@�  @�ƨ@��w@���@�K�@���@�^5@�V@�M�@�E�@�$�@�@��T@��-@�?}@��`@���@��@�Q�@��u@��@�r�@�I�@�1@��P@�dZ@�;d@�;d@�+@�@��@���@���@���@�5?@�@���@��@��#@��h@�X@�&�@��`@��9@���@��u@�bN@�9X@�b@�b@�1@�  @�w@\)@~ȴ@~ff@}�@}�T@}�@}��@}�h@}/@|�/@|z�@|1@{C�@z��@z-@y��@yX@y%@xĜ@x�u@x �@w�w@v�R@v�+@vE�@v$�@u�@u/@tj@t1@st�@s33@s"�@r�H@r�\@rM�@r^5@r�\@rM�@q�^@qhs@qX@qhs@r�@q�@q�^@qhs@p��@pbN@o�;@o�P@o+@nȴ@m��@m/@l�/@l�@l�D@lz�@lj@l�@l1@l1@k�m@k�
@kƨ@k�F@k��@k��@k��@kdZ@kS�@kC�@j��@j^5@j-@iG�@i%@hĜ@h �@g��@g;d@g;d@g�@f�+@e�T@e/@d9X@d(�@d�@c��@c�
@c�F@c�@cdZ@cC�@b�H@b^5@a��@a�7@aG�@`��@` �@_��@_K�@_+@^ȴ@^5?@]��@]`B@]�@\�@\�D@\�@[�m@[��@[t�@Z��@Z~�@Y��@Yx�@X��@X��@X  @W\)@W+@W
=@V�R@VE�@V$�@V{@U��@Up�@U`B@U`B@U`B@UV@T�j@Tj@T9X@T1@St�@R�!@Q�#@Qx�@QX@QG�@Q7L@Q&�@Q&�@P��@P��@PA�@O�@O��@O�@O\)@O�@N��@N�+@NE�@M�@M@M�h@Mp�@L�@L�@K��@K�@K�@K33@J�H@Ko@K"�@J��@J~�@Jn�@I7L@H�9@Hr�@HA�@G��@G�P@G\)@G+@G
=@F�y@Fv�@F{@Ep�@E?}@D��@DI�@C��@C��@CS�@C@B�!@B^5@B�@B�@Bn�@B�!@Bn�@B=q@A�#@A��@A&�@@�`@@r�@@b@?�;@?�w@?�@?�P@?K�@>��@>�+@=��@=`B@=/@<�@<�@<Z@;��@;�
@;ƨ@;o@:�@:�!@:�!@:^5@:�@9�^@9G�@9�@8��@8�`@8�u@8�@8Q�@8 �@8b@8  @7�@7��@7�@6�y@6ȴ@6V@5��@5V@4��@4�j@4j@3��@3��@3"�@2�!@2n�@2^5@2�@1�^@1x�@1�@0�9@0�@01'@/�@/�P@.��@.�R@.��@.��@.��@.��@.�+@.V@.@-?}@,�j@,��@,z�@,Z@,(�@+�m@+��@+S�@*�H@*n�@*=q@)��@)�#@)��@)��@)��@)�7@)&�@(�9@(�@(Q�@( �@(b@(b@'�;@'�@'�P@'K�@'�@&�@&ȴ@&��@&��@&v�@&V@&E�@&5?@&$�@%�@%@%�h@%p�@%`B@%?}@$��@$�@$��@$I�@$(�@$�@#��@#�m@#��@#�@#"�@"��@"^5@"-@!��@!��@!X@!G�@!�@ �`@ �9@ Q�@ A�@ 1'@  �@  �@ b@   @�@�w@|�@l�@�@��@v�@5?@$�@{@��@�h@�@p�@p�@p�@`B@/@��@�/@�@j@I�@9X@�@�m@dZ@S�@S�@S�@S�@C�@C�@C�@33@o@�@^5@�@J@��@�#@��@�7@7L@%@�9@�@bN@Q�@ �@  @�@�w@|�@l�@\)@K�@;d@
=@�@�R@V@@@��@�@/@V@�@�j@9X@�m@�F@dZ@@�@�H@��@��@�\@n�@M�@-@�@��@��@�7@X@G�@�@�9@�u@r�@1'@��@��@|�@;d@�@�y@ȴ@��@V@E�@5?@{@�@�-@O�@��@�@z�@9X@9X@�m@��@�@�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�!B	�B	��B	��B	��B	�'B	�'B	�!B	�!B	�!B	�B	�B	�B	�B	�B	��B	��B	�B	�FB	�qB	��B
�B
�B
 �B
%�B
%�B
#�B
�B
oB

=B	�B	��B	��B	ȴB	��B	�B	�B	�
B	��B	��B	�B	�fB
+B
�B
�B
�B
�B
:^B
M�B
\)B
cTB
gmB
u�B
u�B
ffB
n�B
��B
�^B
ĜB
��B
�#B
�HB
�B
�B�B1'B=qBJ�BffBt�B��B��B��B��B��B��B��B�PB~�BaHB7LB�B'�B8RB7LBI�BXBA�B��B�Bq�B>wBA�B[#BdZBQ�BP�B;dB%B
�5B
�fB
�?B
��B
�/BB
��B
�B
��B
�B
�%B
m�B
N�B
1'B
49B
�B
	7B
	7B
%B
B	�B	�)B	��B	��B	��B	��B	u�B	:^B	'�B��B	B�B�TB��B��B�BB��BŢBƨB��B��B��B��B�9B�B��B��B��B��B�B��B�B��B��B��B��B��B�B�-B��B�LBB�qB�'B�B�jB��B��B��B�#B��B��B�3B�uB��B�hB��B��B��B�\B~�B�B�=B��B��B��B��B�uB�oB�DB�%B�=B� B~�B~�Bl�BjBdZBw�Bl�BS�BVBr�Bx�Bs�Bk�Bk�Be`Bo�Bw�Bu�Bt�B~�B~�B� By�Bx�Bq�BjBffBiyBbNB[#B[#B`BBbNB`BB`BB_;BdZBW
BR�B`BBdZBo�Bk�Bo�BjBo�Bz�B}�Bz�By�B� B~�B~�B}�By�Bw�By�B�1B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�9B�3B�'B�'B�9B�RB�}B��B��BBBĜBǮBǮB��B��B��B�B�#B�BB�NB�HB�BB�BB�BB�B�sB�mB�yB�sB�sB�fB�fB�mB�sB�B�B��B	B	bB	%B	B��B��B��B	
=B		7B	
=B	1B	JB	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	(�B	,B	1'B	2-B	33B	6FB	6FB	6FB	8RB	<jB	@�B	A�B	E�B	H�B	J�B	M�B	O�B	Q�B	R�B	XB	[#B	aHB	aHB	cTB	dZB	e`B	hsB	k�B	n�B	q�B	r�B	s�B	s�B	s�B	s�B	r�B	v�B	{�B	z�B	~�B	� B	�B	�B	�B	�B	�=B	�DB	�JB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�?B	�9B	�3B	�?B	�?B	�FB	�LB	�dB	�^B	�RB	�qB	�}B	��B	��B	ÖB	��B	��B	B	ǮB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�#B	�)B	�)B	�/B	�5B	�/B	�#B	�)B	�5B	�HB	�NB	�NB	�NB	�HB	�HB	�NB	�HB	�HB	�BB	�TB	�ZB	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
+B
1B
+B
DB
DB
DB
DB
	7B
	7B
JB
JB
VB
\B
VB
VB
\B
oB
oB
oB
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
"�B
!�B
"�B
 �B
"�B
"�B
"�B
#�B
$�B
&�B
%�B
#�B
#�B
#�B
#�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
'�B
'�B
&�B
'�B
&�B
(�B
'�B
&�B
(�B
+B
,B
+B
+B
)�B
,B
,B
,B
+B
,B
-B
-B
-B
,B
-B
-B
.B
.B
/B
.B
0!B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
7LB
8RB
8RB
7LB
7LB
7LB
7LB
7LB
5?B
33B
49B
6FB
9XB
:^B
9XB
:^B
:^B
9XB
9XB
9XB
:^B
;dB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
;dB
;dB
:^B
8RB
:^B
;dB
<jB
<jB
<jB
=qB
>wB
=qB
<jB
<jB
:^B
<jB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
=qB
<jB
<jB
>wB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
B�B
B�B
E�B
G�B
H�B
F�B
F�B
F�B
G�B
G�B
H�B
G�B
H�B
I�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
H�B
I�B
J�B
J�B
J�B
I�B
J�B
K�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
N�B
N�B
M�B
O�B
Q�B
O�B
P�B
N�B
P�B
S�B
R�B
R�B
R�B
R�B
S�B
T�B
VB
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
VB
VB
XB
ZB
ZB
ZB
ZB
ZB
YB
XB
W
B
YB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
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
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
dZB
e`B
dZB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
e`B
e`B
ffB
e`B
e`B
ffB
gmB
gmB
hsB
gmB
gmB
hsB
iyB
iyB
iyB
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
hsB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
jB
iyB
hsB
jB
k�B
l�B
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
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
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
s�B
s�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
w�B
w�B
v�B
v�B
v�B
v�B
u�B
x�B
x�B
x�B
y�B
x�B
x�B
y�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�UB	�}B	��B	�HB	��B	�AB	�[B	�;B	�UB	�;B	�5B	�5B	�/B	�=B	�QB	�eB	�KB	�OB	�`B	��B	�8B
�B
�B
 �B
%�B
%�B
$&B
VB
,B
~B	�FB	��B	āB	��B	��B	�kB	خB	׍B	��B	�&B	یB	�XB
1B
EB
B
WB
�B
:�B
N<B
\�B
c�B
h�B
v`B
vFB
h�B
r-B
��B
��B
��B
�.B
�xB
��B
��B
�|B�B2GB>�BLdBg�BvFB�#B�*B�_B�eB�mB�TB�xB��B��Be�B<PB��B%FB9�B:*BK�BZBESB �B��Bv�BC�BCaB\]BezBU2BR�B>�B�B
��G�O�G�O�B
�~B
�5BB
�dB
�MB
��B
��B
�JB
r�B
S[B
5?B
6B
�B
~B
�B
�B
�B	�B	�;B	ԯB	��B	�*B	�NB	{B	A�B	-�B	 OB		B��B�XB�B�NB�bB�oB��BȀB�>B��B�8B��B��B�wB��B�`B�LB��B�/B��B��B��B�ZB�KB�,B�jB��B�hB��B��B�GB��B�hB��B�]B�&B�aB�9B�B�MB��B�fB��B��B��B��B�jB�QB��B��B��B��B�B�9B��B�B�FB�@B��B��B��B��B��B��Bo�BmCBf�BxlBnBW�BXBsByXBt�BmwBmBgBpoBxBvzBu�BcBcB��Bz�By�Br�Bl=Bg�BjBc�B\�B\�BaBc Ba|BabB`\Be`BX�BUBabBe`Bp;Bl�Bp�Bk�Bp�B{0B~BB{�BzxB�iB}B�B~�Bz�By>B{dB��B��B�B�?B�B�dB�eB�WB�BB�TB�`B��B�RB��B�'B�!B�]B�B��B�nB��B��B��B��B��B��B��B��B��B��B�B�1BȀB�bB�aBѷB�yB�qB�\B�B��B��B��B�HB��B��B��B��B��B��B�B��B�
B�B��B��B�B	�B	�B	�B	�B�zB��B��B	
�B		�B	
�B		B	�B	�B	�B	�B	�B	�B	�B	"B	# B	%,B	)_B	,WB	1[B	2|B	3hB	6`B	6zB	6�B	8�B	<�B	@�B	A�B	E�B	H�B	J�B	NB	P.B	R B	S@B	XEB	[qB	aHB	abB	cnB	d�B	e�B	h�B	k�B	n�B	q�B	r�B	s�B	s�B	s�B	s�B	r�B	v�B	|B	{0B	.B	�4B	�;B	�AB	�aB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�B	�
B	�$B	�KB	�)B	�5B	�5B	�;B	�AB	�AB	�AB	�MB	�TB	�ZB	�TB	��B	�tB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	ÖB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	� B	�B	�B	� B	�HB	�&B	�&B	�4B	�NB	�@B	�,B	�2B	�FB	�uB	�aB	�YB	�EB	�YB	�QB	�WB	�]B	ܒB	�IB	�OB	�~B	یB	�xB	ޞB	�|B	�NB	�hB	�B	�|B	�|B	�B	�B	�B	�B	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�	B	�B	�+B	�	B	��B	��B	��B	�	B	��B	�B	��B	�	B	�B	��B	�B	��B	�B	�	B	�	B	�B	�B	�B	�(B	�(B	�(B	�B
 B
 B
AB
'B
;B
UB
 OB
;B
AB
3B
9B
9B
3B
gB
MB
MB
gB
gB
SB
SB
YB
tB
EB
EB
fB
_B
fB
�B
^B
^B
^B
^B
	�B
	�B
~B
~B
pB
�B
pB
pB
�B
oB
�B
�B
�B
�B
{B
�B
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
#B
"�B
#�B
#�B
#�B
$�B
$�B
$B
$B
#�B
#B
"B
#B
!B
#B
# B
#B
$B
%B
'B
%�B
$&B
$&B
$&B
$@B
(
B
(
B
($B
($B
)B
)*B
)*B
(
B
(>B
'B
(
B
'B
)*B
($B
'8B
)*B
+B
,=B
+6B
+6B
*0B
,"B
,"B
,"B
+6B
,=B
-)B
-)B
-)B
,=B
-]B
-CB
.IB
.IB
/iB
.}B
0oB
2GB
2GB
2aB
2aB
3MB
4TB
4TB
5ZB
7LB
8lB
8RB
7fB
7fB
7fB
7fB
7fB
5tB
3�B
4�B
6`B
9�B
:^B
9rB
:^B
:^B
9rB
9rB
9�B
:�B
;B
:xB
:�B
:xB
:�B
;B
;B
;B
<�B
;B
;B
:�B
8�B
:xB
;B
<�B
<�B
<�B
=qB
>wB
=�B
<�B
<�B
:�B
<�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
>�B
=�B
<�B
<�B
>�B
=�B
=�B
>�B
>�B
?�B
?�B
@�B
B�B
B�B
E�B
G�B
H�B
F�B
F�B
F�B
G�B
G�B
H�B
G�B
H�B
I�B
J�B
J�B
J�B
I�B
IB
H�B
HB
IB
I�B
J�B
J�B
J�B
I�B
J�B
K�B
J	B
J�B
J�B
K�B
J�B
J�B
KB
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
OB
N�B
NB
O�B
R B
PB
QB
OBB
Q B
S�B
S@B
S&B
S&B
S@B
T,B
UB
VB
UB
UB
U2B
VB
VB
V9B
W$B
W$B
V9B
V9B
X+B
ZB
ZB
ZB
Z7B
Z7B
Y1B
XEB
WYB
YKB
[=B
[WB
[=B
[=B
[=B
[=B
[=B
[WB
[WB
]IB
]IB
^5B
^OB
^5B
^OB
]dB
]dB
]dB
^OB
^OB
_VB
_VB
_;B
_VB
_VB
_VB
_VB
_VB
`\B
`\B
`\B
`\B
`\B
aHB
aHB
aHB
abB
`\B
`\B
abB
abB
abB
abB
abB
abB
a|B
abB
bhB
cnB
bhB
b�B
bhB
bhB
a�B
b�B
b�B
cnB
cnB
cnB
dtB
ezB
ezB
dtB
ezB
d�B
ffB
ffB
ffB
g�B
f�B
ffB
f�B
ezB
ezB
f�B
ezB
e�B
f�B
g�B
g�B
h�B
g�B
g�B
h�B
iyB
i�B
iyB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
h�B
jB
k�B
k�B
k�B
k�B
k�B
jB
j�B
j�B
i�B
h�B
j�B
k�B
l�B
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
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
r�B
r�B
r�B
r�B
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
s�B
s�B
t�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
w�B
w�B
v�B
v�B
v�B
v�B
u�B
x�B
x�B
x�B
y�B
x�B
x�B
y�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808250035032018082500350320180825003503201808250200262018082502002620180825020026201808260028562018082600285620180826002856  JA  ARFMdecpA19c                                                                20180821093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180821003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180821003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180821003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180821003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180821003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180821003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180821003526  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180821003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180821003526  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180821003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180821003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180821005556                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180821153424  CV  JULD            G�O�G�O�F�٢                JM  ARSQJMQC2.0                                                                 20180822000000  CF  PSAL_ADJUSTED_QCCN  CN  G�O�                JM  ARSQJMQC2.0                                                                 20180822000000  CF  TEMP_ADJUSTED_QCCN  CN  G�O�                JM  ARCAJMQC2.0                                                                 20180824153503  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180824153503  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180824170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180825152856  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                