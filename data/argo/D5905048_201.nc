CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-16T21:35:12Z creation;2018-01-16T21:35:16Z conversion to V3.1;2019-12-19T07:47:28Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180116213512  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_201                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�E3B.E 1   @�E3���@4����djA�7K�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C�)C!��C#��C%��C'��C)��C+��C-��C/��C2\C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6��D6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn��Do�Do}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw��Dw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�K�A�M�A�S�A�S�A�VA�VA�VA�VA�XA�ZA�XA�VA�XA�XA�XA�M�A�?}A�$�A��AɅA��mA�v�A�JAǃA���A�"�A���Aŗ�A�I�A� �A�bA�  A���A��A��A��/A��
AĶFAğ�A�ZA�(�A�1A���A�ffA�G�A��A��A��A��/A�p�A��A��uA��PA�  A�E�A���A��DA�p�A�;dA���A��-A���A���A�VA�5?A�r�A���A��A���A�\)A�9XA��TA���A�I�A��A��TA�O�A�C�A�$�A��jA�$�A�{A��`A��A�p�A�A��A��A�p�A���A�{A���A�v�A�z�A���A�+A��wA�C�A�Q�A��A�?}A�bA��A�ĜA��wA� �A�(�A�K�A�G�A�oA��RA�7LA��A��!A�A�S�A��^A��FA��7A��!A��A|I�Az��Az5?Ax�yAv�jAs��AqhsApZAm��Al1AkVAj�Ah��Ael�Ad$�A^�RAZ9XAY7LAW|�AU�FAT(�AQp�AO�
AN�`AM�mAMdZAL��AL~�AL�AJ��AJ�\AJbNAJ  AH�uAG�ACƨAB��AB��ABv�AB{AA�PA@A>��A=l�A<$�A:�A9t�A8n�A6ZA5O�A3�^A2z�A1�A0�yA0�uA0-A.��A,�A+t�A*��A)�
A(~�A'�A&  A$�\A"��A"I�A�mAVAM�Ap�A�yA��A7LA�`A1'A��A�hA`BAA�At�AffA�mAC�A�AG�A�AbNA�#A��A�\A�mAS�A
  A	p�A�/A��A��A&�A�9A��At�AVA �jA -@�
=@��@��@��R@��@��@��T@�1'@���@�@�r�@��H@�9@�b@�+@��/@�o@�J@噚@�%@�t�@���@�$�@���@�1@ް!@��@��@�E�@��#@�7L@׾w@�n�@ՙ�@ӕ�@ҏ\@�p�@д9@� �@Ϯ@�o@�v�@�Ĝ@�b@�C�@��@���@� �@��m@ư!@���@î@�v�@�O�@�Q�@�S�@�v�@�x�@��9@��w@��\@��^@���@�I�@�\)@���@�v�@��7@�`B@�&�@��@�9X@�ƨ@��@�n�@�5?@��T@�x�@�?}@��@��9@���@�bN@�1'@� �@���@�l�@�o@���@�-@��@��7@��`@�1'@���@��@�S�@�
=@��@���@�~�@�n�@�n�@�^5@�=q@�J@�O�@�&�@��`@��u@�9X@�  @�ƨ@��@���@��@�K�@��!@�v�@�V@��@��T@���@�@��7@�X@���@��@�j@���@���@�@��y@���@�M�@�@���@��^@���@�?}@���@���@��9@�r�@�(�@�b@���@�|�@�;d@�o@���@�M�@�=q@�-@���@���@��^@��h@�?}@��/@���@�bN@�(�@�b@�  @���@���@�\)@�C�@�;d@���@��!@��+@�M�@�^5@�n�@�n�@�J@�@���@�hs@�Ĝ@��9@�Q�@�bN@�bN@�Q�@�1'@�b@�  @���@��P@�dZ@�K�@�+@�"�@���@���@���@�`B@�x�@��@���@���@�z�@�j@�j@�9X@��@���@�t�@�K�@�;d@�33@�+@��@�@��H@��@��!@���@��+@��+@�n�@��@��T@���@�@�O�@���@��@�I�@�Q�@�ƨ@��P@�C�@�@�v�@�$�@��T@��@��h@��@��@���@�z�@�Z@�Z@�Z@�Q�@�I�@�1'@��m@�|�@�
=@��+@��@�@��#@���@��7@��@�%@���@���@�r�@�(�@�b@��@��
@�ƨ@���@�t�@�dZ@�l�@�l�@�dZ@�o@��@���@���@��\@�M�@�@���@�hs@�&�@���@���@�j@�Q�@�A�@� �@�b@���@��F@�S�@�33@��H@���@�V@�-@�{@��@���@�x�@�p�@�7L@�V@���@��@�A�@�9X@�1@�P@~v�@~5?@~@|�@|�j@|�j@|�@{ƨ@{�F@{S�@z�@z�@z^5@y��@z�@y��@xĜ@xr�@x1'@w�@v��@v{@u�h@up�@u�@t�@tj@s�m@s33@r�H@rn�@r=q@r�@q��@q�#@qhs@p��@p�9@o�@o|�@o;d@n��@n�R@nv�@n5?@m�T@l�/@l��@lz�@lI�@k��@j�!@j~�@j=q@i�^@i&�@hĜ@h��@hb@gK�@f�@f�R@f��@f��@f�+@fV@f$�@e�@e@e�h@e`B@eO�@e�@d��@d�D@d�@c�F@cdZ@cS�@c@b��@b��@b^5@b�@a��@a�^@aG�@a&�@a�@`bN@` �@_�P@_�@^��@^�y@^�R@^�+@^V@^5?@]�@]`B@]�@\��@\�/@\�j@\z�@\9X@\1@[�
@[�@[S�@[C�@["�@[o@[o@Z��@Z=q@Yx�@YG�@Y&�@Y%@X��@XQ�@X �@W�@W�;@W��@WK�@W�@V��@V�@Vȴ@V��@V{@U�-@T��@T��@Tj@TZ@T9X@S�m@Sƨ@S�@SdZ@SS�@S"�@R�H@R��@R�\@Rn�@RM�@RJ@Q�@Q�^@QG�@PĜ@P�@PQ�@O�w@OK�@O
=@N�@N��@N5?@M�h@M�h@MO�@L��@LI�@K�m@K��@KS�@JJ@I�7@Ihs@I�@H��@HĜ@H��@H��@H�@HQ�@H �@Hb@G�@G��@G�@G��@Gl�@F�@F�+@FE�@F@E�-@E?}@D�@D��@DZ@C��@C@B�H@B��@B~�@B-@Ax�@A7L@A%@@��@@��@@��@@bN@?�;@?�P@?K�@?
=@>ȴ@>��@>5?@=�T@=?}@=�@<��@<z�@<j@<(�@;ƨ@;��@;o@:n�@:�@9��@9�^@9x�@9&�@8�`@8��@8Q�@8 �@7�;@7�w@7�@7��@7|�@6ȴ@6��@6v�@6{@5�-@5�h@5p�@5O�@5?}@4�@41@3ƨ@3ƨ@3��@3dZ@333@2�H@2�H@2�!@2~�@2-@1�@1hs@1&�@0��@0��@0A�@/�@/�P@/\)@/+@/�@.��@.�@.��@.5?@-�@-�-@-`B@,��@,��@,�@,�D@,j@,j@,I�@,(�@,�@+�m@+��@+t�@+o@*��@*~�@*^5@*-@)�^@)��@)hs@)G�@)7L@)�@)%@(�`@(�9@(�u@(Q�@(  @'�@'�@&��@&�+@&v�@&ff@&ff@&ff@&V@&{@%?}@$�@$��@$�j@$�D@$z�@$�@$1@#��@#�
@#ƨ@#��@#�@#S�@#o@"��@"n�@"�@!�#@!��@!hs@!7L@!&�@!�@!�@!%@ ��@ ��@ ��@ �@ Q�@ A�@ b@��@�P@l�@\)@��@�R@��@��@�+@v�@V@5?@�@�-@p�@O�@?}@�@��@�@�j@��@z�@I�@�@�m@ƨ@��@�@S�@"�@�@��@�H@��@�\@=q@J@J@��@��@&�@��@�@�@r�@�@�w@�P@l�@;d@
=@�@ȴ@v�@ff@{@�-@�@O�@�@�/@�D@z�@j@I�@(�@�
@��@�@�@dZ@"�@��@��@n�@J@�#@x�@hs@X@G�@�@%@��@��@��@�u@bN@A�@  @�;@�;@�@|�@l�@K�@+@�R@$�@�T@�-@�@?}@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�K�A�M�A�S�A�S�A�VA�VA�VA�VA�XA�ZA�XA�VA�XA�XA�XA�M�A�?}A�$�A��AɅA��mA�v�A�JAǃA���A�"�A���Aŗ�A�I�A� �A�bA�  A���A��A��A��/A��
AĶFAğ�A�ZA�(�A�1A���A�ffA�G�A��A��A��A��/A�p�A��A��uA��PA�  A�E�A���A��DA�p�A�;dA���A��-A���A���A�VA�5?A�r�A���A��A���A�\)A�9XA��TA���A�I�A��A��TA�O�A�C�A�$�A��jA�$�A�{A��`A��A�p�A�A��A��A�p�A���A�{A���A�v�A�z�A���A�+A��wA�C�A�Q�A��A�?}A�bA��A�ĜA��wA� �A�(�A�K�A�G�A�oA��RA�7LA��A��!A�A�S�A��^A��FA��7A��!A��A|I�Az��Az5?Ax�yAv�jAs��AqhsApZAm��Al1AkVAj�Ah��Ael�Ad$�A^�RAZ9XAY7LAW|�AU�FAT(�AQp�AO�
AN�`AM�mAMdZAL��AL~�AL�AJ��AJ�\AJbNAJ  AH�uAG�ACƨAB��AB��ABv�AB{AA�PA@A>��A=l�A<$�A:�A9t�A8n�A6ZA5O�A3�^A2z�A1�A0�yA0�uA0-A.��A,�A+t�A*��A)�
A(~�A'�A&  A$�\A"��A"I�A�mAVAM�Ap�A�yA��A7LA�`A1'A��A�hA`BAA�At�AffA�mAC�A�AG�A�AbNA�#A��A�\A�mAS�A
  A	p�A�/A��A��A&�A�9A��At�AVA �jA -@�
=@��@��@��R@��@��@��T@�1'@���@�@�r�@��H@�9@�b@�+@��/@�o@�J@噚@�%@�t�@���@�$�@���@�1@ް!@��@��@�E�@��#@�7L@׾w@�n�@ՙ�@ӕ�@ҏ\@�p�@д9@� �@Ϯ@�o@�v�@�Ĝ@�b@�C�@��@���@� �@��m@ư!@���@î@�v�@�O�@�Q�@�S�@�v�@�x�@��9@��w@��\@��^@���@�I�@�\)@���@�v�@��7@�`B@�&�@��@�9X@�ƨ@��@�n�@�5?@��T@�x�@�?}@��@��9@���@�bN@�1'@� �@���@�l�@�o@���@�-@��@��7@��`@�1'@���@��@�S�@�
=@��@���@�~�@�n�@�n�@�^5@�=q@�J@�O�@�&�@��`@��u@�9X@�  @�ƨ@��@���@��@�K�@��!@�v�@�V@��@��T@���@�@��7@�X@���@��@�j@���@���@�@��y@���@�M�@�@���@��^@���@�?}@���@���@��9@�r�@�(�@�b@���@�|�@�;d@�o@���@�M�@�=q@�-@���@���@��^@��h@�?}@��/@���@�bN@�(�@�b@�  @���@���@�\)@�C�@�;d@���@��!@��+@�M�@�^5@�n�@�n�@�J@�@���@�hs@�Ĝ@��9@�Q�@�bN@�bN@�Q�@�1'@�b@�  @���@��P@�dZ@�K�@�+@�"�@���@���@���@�`B@�x�@��@���@���@�z�@�j@�j@�9X@��@���@�t�@�K�@�;d@�33@�+@��@�@��H@��@��!@���@��+@��+@�n�@��@��T@���@�@�O�@���@��@�I�@�Q�@�ƨ@��P@�C�@�@�v�@�$�@��T@��@��h@��@��@���@�z�@�Z@�Z@�Z@�Q�@�I�@�1'@��m@�|�@�
=@��+@��@�@��#@���@��7@��@�%@���@���@�r�@�(�@�b@��@��
@�ƨ@���@�t�@�dZ@�l�@�l�@�dZ@�o@��@���@���@��\@�M�@�@���@�hs@�&�@���@���@�j@�Q�@�A�@� �@�b@���@��F@�S�@�33@��H@���@�V@�-@�{@��@���@�x�@�p�@�7L@�V@���@��@�A�@�9X@�1@�P@~v�@~5?@~@|�@|�j@|�j@|�@{ƨ@{�F@{S�@z�@z�@z^5@y��@z�G�O�@xĜ@xr�@x1'@w�@v��@v{@u�h@up�@u�@t�@tj@s�m@s33@r�H@rn�@r=q@r�@q��@q�#@qhs@p��@p�9@o�@o|�@o;d@n��@n�R@nv�@n5?@m�T@l�/@l��@lz�@lI�@k��@j�!@j~�@j=q@i�^@i&�@hĜ@h��@hb@gK�@f�@f�R@f��@f��@f�+@fV@f$�@e�@e@e�h@e`B@eO�@e�@d��@d�D@d�@c�F@cdZ@cS�@c@b��@b��@b^5@b�@a��@a�^@aG�@a&�@a�@`bN@` �@_�P@_�@^��@^�y@^�R@^�+@^V@^5?@]�@]`B@]�@\��@\�/@\�j@\z�@\9X@\1@[�
@[�@[S�@[C�@["�@[o@[o@Z��@Z=q@Yx�@YG�@Y&�@Y%@X��@XQ�@X �@W�@W�;@W��@WK�@W�@V��@V�@Vȴ@V��@V{@U�-@T��@T��@Tj@TZ@T9X@S�m@Sƨ@S�@SdZ@SS�@S"�@R�H@R��@R�\@Rn�@RM�@RJ@Q�@Q�^@QG�@PĜ@P�@PQ�@O�w@OK�@O
=@N�@N��@N5?@M�h@M�h@MO�@L��@LI�@K�m@K��@KS�@JJ@I�7@Ihs@I�@H��@HĜ@H��@H��@H�@HQ�@H �@Hb@G�@G��@G�@G��@Gl�@F�@F�+@FE�@F@E�-@E?}@D�@D��@DZ@C��@C@B�H@B��@B~�@B-@Ax�@A7L@A%@@��@@��@@��@@bN@?�;@?�P@?K�@?
=@>ȴ@>��@>5?@=�T@=?}@=�@<��@<z�@<j@<(�@;ƨ@;��@;o@:n�@:�@9��@9�^@9x�@9&�@8�`@8��@8Q�@8 �@7�;@7�w@7�@7��@7|�@6ȴ@6��@6v�@6{@5�-@5�h@5p�@5O�@5?}@4�@41@3ƨ@3ƨ@3��@3dZ@333@2�H@2�H@2�!@2~�@2-@1�@1hs@1&�@0��@0��@0A�@/�@/�P@/\)@/+@/�@.��@.�@.��@.5?@-�@-�-@-`B@,��@,��@,�@,�D@,j@,j@,I�@,(�@,�@+�m@+��@+t�@+o@*��@*~�@*^5@*-@)�^@)��@)hs@)G�@)7L@)�@)%@(�`@(�9@(�u@(Q�@(  @'�@'�@&��@&�+@&v�@&ff@&ff@&ff@&V@&{@%?}@$�@$��@$�j@$�D@$z�@$�@$1@#��@#�
@#ƨ@#��@#�@#S�@#o@"��@"n�@"�@!�#@!��@!hs@!7L@!&�@!�@!�@!%@ ��@ ��@ ��@ �@ Q�@ A�@ b@��@�P@l�@\)@��@�R@��@��@�+@v�@V@5?@�@�-@p�@O�@?}@�@��@�@�j@��@z�@I�@�@�m@ƨ@��@�@S�@"�@�@��@�H@��@�\@=q@J@J@��@��@&�@��@�@�@r�@�@�w@�P@l�@;d@
=@�@ȴ@v�@ff@{@�-@�@O�@�@�/@�D@z�@j@I�@(�@�
@��@�@�@dZ@"�@��@��@n�@J@�#@x�@hs@X@G�@�@%@��@��@��@�u@bN@A�@  @�;@�;@�@|�@l�@K�@+@�R@$�@�T@�-@�@?}@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BXBW
BXBW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BVBVBXBZB_;BdZBu�B�oB��B�LB��B�)B�B�B��BDBPBVB\BPBDBJBbBhB�B�B�B�B �B"�B-B/B2-B:^B8RB49B=qB>wB>wBcTBo�B� B�B�1B�B{�Bu�Bu�BiyBD�B>wB8RB=qB]/BaHBYBQ�BdZBdZBbNBaHBXBL�BB�B7LBB�B7LB#�BuBPBVB	7BB�B�/BB�dB�XB�B�DBaHBVBe`B[#BE�B;dB49B33B,B�BB
�B
�^B
��B
ŢB
�XB
B
�RB
�B
��B
�hB
�B
w�B
_;B
H�B
.B
�B
B
{B
�B
JB	�B	�)B	�B	�
B	B	�^B	�wB	�?B	��B	q�B	n�B	B�B	!�B	H�B	6FB	(�B	'�B	
=B	�B	�B	�B	�B	�B	uB	bB	+B	
=B	1B��B�B�HBȴB�ZB�yB�sB�TB�/B��BɺBB�jB�RB�!B�-B��B��B��B��B��B��B��B��B�1B{�B|�B�B�Bv�Bs�Bm�Be`BdZBl�BXBYBYBffBjBffBe`BhsB`BBVB\)BffB_;B^5B^5BaHB_;BYB_;BffBe`BcTBdZB[#B[#B]/BT�B]/B\)BR�BL�BW
BYBL�BJ�B]/B_;B[#BYBS�BYBbNB\)BS�BP�BO�BF�BVBW
BM�BE�BN�BJ�BF�BF�BK�BO�BN�BI�BM�BM�BJ�BP�BR�BS�BZBZBbNBaHB\)B^5BdZB^5BjBl�Bs�Bu�Bv�Bu�Bt�Bn�Bx�By�Bz�B~�B�+B�7B�B�=B�B�PB�uB��B��B��B��B��B��B�B�'B�FB�^B�qBB��BȴB��B��B��B��B�
B�B�;B�`B�`B�mB�B�B�B�B�B��B��B��B��B��B��B	B	+B	1B		7B	\B	�B	�B	�B	 �B	#�B	%�B	(�B	+B	+B	+B	)�B	+B	)�B	1'B	33B	49B	6FB	9XB	<jB	>wB	?}B	?}B	?}B	>wB	C�B	E�B	F�B	K�B	M�B	N�B	N�B	P�B	Q�B	S�B	]/B	aHB	ffB	gmB	m�B	l�B	l�B	n�B	r�B	s�B	s�B	r�B	u�B	x�B	y�B	z�B	� B	�B	�B	�B	�7B	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�'B	�-B	�?B	�LB	�XB	�jB	�jB	�^B	�^B	�jB	�qB	�dB	��B	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�BB	�BB	�;B	�;B	�BB	�NB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�sB	�sB	�sB	�sB	�fB	�mB	�sB	�sB	�fB	�`B	�fB	�sB	�B	�sB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
1B
1B
+B
%B
%B
1B
	7B
	7B
DB
DB
DB
DB
DB
DB

=B

=B
	7B
JB
JB
PB
PB
VB
\B
VB
VB
bB
hB
bB
hB
hB
hB
bB
uB
oB
hB
bB
{B
{B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
#�B
$�B
#�B
"�B
!�B
%�B
%�B
$�B
%�B
&�B
&�B
%�B
%�B
'�B
)�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
.B
-B
-B
.B
.B
,B
.B
-B
/B
0!B
1'B
0!B
0!B
1'B
1'B
0!B
/B
1'B
2-B
2-B
2-B
1'B
2-B
2-B
2-B
2-B
33B
49B
33B
49B
33B
2-B
1'B
1'B
49B
5?B
5?B
49B
49B
5?B
5?B
6FB
5?B
5?B
6FB
7LB
7LB
6FB
6FB
49B
6FB
5?B
7LB
8RB
9XB
9XB
8RB
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
:^B
:^B
:^B
:^B
<jB
;dB
;dB
;dB
=qB
=qB
=qB
<jB
<jB
?}B
>wB
=qB
>wB
?}B
?}B
>wB
<jB
@�B
C�B
B�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
E�B
H�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
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
I�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
K�B
K�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
Q�B
P�B
P�B
N�B
P�B
P�B
P�B
P�B
Q�B
R�B
Q�B
R�B
P�B
P�B
R�B
T�B
T�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
T�B
VB
VB
W
B
VB
VB
XB
YB
YB
ZB
ZB
YB
YB
XB
YB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
^5B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
aHB
aHB
aHB
aHB
aHB
`BB
_;B
^5B
`BB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
e`B
e`B
dZB
e`B
ffB
ffB
e`B
ffB
ffB
ffB
ffB
ffB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
jB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
jB
jB
k�B
l�B
k�B
jB
jB
k�B
m�B
m�B
m�B
k�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
p�B
p�B
o�B
p�B
p�B
o�B
p�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
v�B
v�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW�BW
BXBW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BVBVBXEBZ�B_�BezBwB�[B��B��B�@B�dB�B�3B�BxBjBpB\BjB^BJB}B�B�BBBB!bB#�B-CB/�B2|B:xB8�B5?B>BB?�B@iBdtBqB��B��B��B��B}�BxBxBmBJ=BB�B<B@�B^�Bb�B[�BS�Bd�Bd�BcBa�BX�BNpBD�B9�BC�B8�B&fBBBB�B
rB�B�-B�-B��B��B�PB��B�HBg8BY1BfLB\�BG�B<�B5%B3�B,�B �B�B
�B
�]B
��B
��B
��B
�{B
��B
��B
��B
�B
��B
y�B
a�B
LB
2�B
�B

XB
9B
�B
VB	��B	��B	ܒB	��B	�mB	��B	��B	�`B	�vB	vB	qAB	H�B	&�B	J#B	8�B	+�B	*B	PB	EB	�B	�B	KB	1B	FB	B	�B	
�B	�B��B�B�B�JB�B��B��B�&B�5B� B�^BĜB�(B�B�B��B�-B�TB��B�QB��B�xB�]B��B�#B~]B~�B�AB�[Bx�Bu?Bo�Bg�Bf�Bm�B[#B[WB[�Bg�BkkBg�BffBi*Ba�BXB]~Bf�B`�B_�B_�BbB`\BZ�B`\BgBfBd&Bd�B\�B\)B^5BV�B^B]IBT�BN�BXEBZBO(BL�B]�B_�B\)BZ7BU�BZ7Bb�B]BU�BRTBQ4BH�BVSBW�BO(BG+BOvBLBG�BG�BL~BPHBO�BJ�BNVBNpBK�BQ�BS�BU2B[	B[=Bb�Ba�B]IB_;BeFB_�BkQBm]BtTBvFBw2Bv`BuZBo�ByrBz�B{�B�B��B��B�B�B��B�<B�aB�_B�]B�pB��B��B��B��B��B��B��B�(B�B��BɆB� B�@B�TBՁB�sBٴBߤB�B�B�B�B��B��B��B��B�B��B�B�2B�DB�PB	�B	_B	�B		�B	�B	�B	�B	B	 �B	$&B	&B	)B	+B	+B	+B	*0B	+QB	*B	1AB	3hB	4�B	6�B	9�B	<�B	>�B	?�B	?�B	?�B	>�B	C�B	E�B	GB	K�B	M�B	N�B	OB	Q4B	R:B	TaB	]~B	a�B	f�B	g�B	m�B	l�B	l�B	n�B	r�B	s�B	s�B	sB	u�B	x�B	zB	{0B	�4B	�MB	�SB	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�>B	�B	�)B	�)B	�WB	�IB	�AB	�GB	�[B	�aB	�tB	�fB	�XB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�B	�B	�B	�&B	�NB	�}B	�2B	�B	�kB	�QB	�xB	�VB	�vB	�\B	�pB	ߊB	��B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�%B	�%B	�FB	�2B	�B	�*B	�*B	�B	�B	�*B	�B	�"B	�<B	�(B
 4B
;B
 B
'B
'B
3B
?B
+B
1B
fB
fB
fB
	RB
KB
fB
_B
tB
�B
fB
	lB
	�B
xB
^B
^B
^B
^B
^B

XB

rB
	�B
dB
~B
�B
�B
�B
vB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	G�O�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
 �B
 �B
!�B
!�B
!�B
 �B
 'B
$B
$�B
$B
# B
"B
&B
&B
%B
&2B
'B
'B
&B
&2B
(
B
)�B
)�B
)�B
*0B
*0B
)B
*B
*B
*B
*B
+B
*B
*B
*B
*0B
+6B
,"B
,"B
,"B
-)B
-)B
-)B
-)B
./B
-)B
-CB
.IB
./B
,WB
./B
-CB
/5B
0;B
1'B
0UB
0UB
1AB
1AB
0;B
/OB
1AB
2GB
2GB
2GB
1AB
2GB
2GB
2GB
2GB
3MB
49B
3MB
4TB
3MB
2aB
1[B
1�B
4nB
5ZB
5tB
4nB
4nB
5ZB
5ZB
6`B
5ZB
5ZB
6`B
7fB
7fB
6`B
6`B
4nB
6zB
5�B
7fB
8lB
9rB
9rB
8lB
9rB
9rB
:�B
:xB
:xB
:xB
:xB
;B
;B
;B
:xB
:xB
:xB
:�B
:�B
<�B
;B
;�B
;�B
=�B
=�B
=�B
<�B
<�B
?}B
>�B
=�B
>�B
?�B
?�B
>�B
<�B
@�B
C�B
B�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
E�B
H�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
KB
J�B
J	B
K�B
K�B
L�B
MB
L�B
MB
M�B
LB
K�B
M�B
M�B
N�B
OB
NB
N�B
N�B
OB
O�B
O�B
Q B
Q�B
Q B
QB
OBB
Q B
Q B
QB
QB
R B
S&B
RB
SB
QB
QB
SB
T�B
U2B
UB
UB
UB
VB
VB
UB
UB
UB
UMB
VB
VB
W$B
VSB
V9B
X+B
Y1B
Y1B
Z7B
Z7B
Y1B
Y1B
XEB
Y1B
ZQB
Z7B
Z7B
[=B
\CB
\CB
\CB
\CB
\CB
\CB
\CB
\]B
\]B
[WB
\]B
\CB
]IB
]IB
]IB
]IB
^5B
^jB
^OB
_;B
^OB
_;B
^OB
^OB
^OB
^jB
^jB
^OB
]dB
^jB
aHB
abB
aHB
aHB
abB
`\B
_�B
^�B
`vB
bhB
bhB
b�B
b�B
bhB
cTB
cTB
cnB
cnB
cnB
c�B
cnB
bhB
bhB
b�B
cnB
dtB
dtB
ezB
e�B
ezB
ffB
f�B
ffB
ffB
e`B
e�B
dtB
e�B
f�B
f�B
ezB
f�B
f�B
f�B
f�B
f�B
hsB
h�B
hsB
hsB
g�B
g�B
g�B
g�B
h�B
h�B
iyB
h�B
h�B
iyB
h�B
i�B
i�B
i�B
i�B
i�B
j�B
i�B
jB
j�B
j�B
j�B
k�B
k�B
k�B
j�B
j�B
k�B
l�B
k�B
j�B
j�B
k�B
m�B
m�B
m�B
k�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
p�B
p�B
o�B
p�B
p�B
o�B
p�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
v�B
v�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801210035512018012100355120180121003551201806221325002018062213250020180622132500201804050728222018040507282220180405072822  JA  ARFMdecpA19c                                                                20180117063511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180116213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180116213514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180116213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180116213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180116213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180116213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180116213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180116213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180116213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20180116215527                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180117153413  CV  JULD            G�O�G�O�F�)�                JM  ARSQJMQC2.0                                                                 20180118000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARSQJMQC2.0                                                                 20180118000000  CF  TEMP_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20180120153551  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180120153551  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222822  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042500  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                