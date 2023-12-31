CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-22T00:35:19Z creation;2017-09-22T00:35:22Z conversion to V3.1;2019-12-19T07:56:55Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170922003519  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_162                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�'�΁� 1   @�'�8�@4�$�/��d�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D��3D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�fD�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDS�DS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�D���D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�D�(R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�C�A�E�A�G�A�I�A�I�A�I�A�I�A�I�A�C�A�5?A�-A��;A��TA��A�K�A�|�A߲-A�1A޲-A�9XAݙ�A�=qA�Q�A���A׏\A�p�A�x�A�C�A�G�A�33A�9XA�$�A�VA΋DA�{A˴9Aɟ�A�hsA�~�A�1'AčPA�I�A�Q�A�A��A���A�5?A�\)A���A� �A��7A�&�A�hsA���A�7LA�/A�G�A��A���A�(�A�z�A�&�A�ĜA�M�A�A�l�A�M�A�
=A�?}A�I�A��wA�r�A�
=A��#A��DA���A�$�A�A���A��A���A��7A�\)A�bNA�;dA�JA�S�A�(�A�Q�A�7LA��mA��A��A�=qA���A�bNA��A��A�ĜA��-A���A���A��TA���A���A��yA���A�v�A�ZA��A���A�%A~1A{�;Ay�hAxȴAw7LAv1AsƨAq�wApr�AoAn��AmƨAk�PAi��AiK�AiAh�HAh~�AghsAf��AfVAdr�Ac�Aa`BA_oA]�A[�AZ^5AY�TAYVAW`BASAP~�AO/AN�uAMAK��AF�DAE��AEXAE/ADȴAA�A?�#A>��A=33A<�\A<(�A;�
A;�PA:1A8�+A65?A4��A41'A4A3S�A2��A2ZA1oA0I�A/��A.��A-�A-oA,��A*��A*jA)�A(5?A'dZA& �A#�;A!�A�A�uAXAI�A�/A��Az�A�A�jA�FAA�HA�!AA�At�AA1'A"�A$�A\)AbNA�#A��Ax�A;dA�9A�A
5?A	p�A�`At�A��A�\A-A�hA?}A`BAx�A1'A�A��A1Ap�@�\)@��@���@�z�@��@�7@�%@���@�S�@��@��@�$�@�@�9X@�@�b@�-@���@�O�@�S�@��@�K�@�Z@�`B@�u@�\@�ff@�$�@�n�@��@�!@���@��m@�+@ޟ�@ޟ�@�5?@ݡ�@ܣ�@ۮ@ڏ\@ٲ-@�b@�@���@�Z@���@ͩ�@�G�@��@��@�S�@�@�z�@ǅ@���@�v�@Ų-@�%@ļj@�1@�|�@�C�@��@°!@�~�@���@�?}@��`@�z�@�=q@�@��^@���@�1@�1@��m@�|�@��y@��@��`@���@�z�@�9X@���@�@�v�@��/@��m@��;@��F@��P@�|�@�l�@�\)@�K�@�+@��H@��+@�{@���@���@���@�-@�M�@�M�@�`B@��u@�A�@�  @��@���@�ff@��h@��@��9@�r�@���@���@�\)@�;d@���@���@��\@�5?@���@�x�@�X@��@�bN@�1@��
@�|�@�"�@��R@���@�~�@��T@�`B@��@��/@��j@��@��;@��@�dZ@�;d@�+@�"�@�
=@�@���@��@���@�V@�M�@�5?@��@��7@�j@�Q�@�Q�@�I�@�I�@�A�@� �@��m@��w@��F@���@��P@�K�@�o@��@��!@�ff@�=q@�@���@�O�@��@���@�Ĝ@��9@��u@�r�@�9X@�A�@�1@��F@�l�@�C�@�+@���@��!@�{@���@�`B@��@�V@��`@���@���@��u@�A�@��F@���@�t�@�;d@�o@��@��H@��H@��H@��@��\@�ff@�=q@���@�hs@�&�@��/@��u@�j@��@��m@��@�S�@�;d@�@��!@�=q@�J@���@��@�@��@�G�@�&�@���@���@��u@���@�j@��@�t�@�K�@�+@�
=@��@���@�E�@��-@��@�hs@�G�@��/@�1'@�  @�ƨ@���@�dZ@�"�@��H@���@�=q@���@���@��7@��@��@�r�@�A�@�(�@��@���@��F@���@�dZ@�+@��@�ff@�$�@��@�X@�G�@��@��`@��j@��u@���@���@�dZ@�"�@��@���@�@��h@�p�@�hs@�X@�/@��j@�bN@�A�@�b@�@�P@~�y@~5?@}�-@}O�@}V@|�/@|�D@|j@|9X@{��@{�F@{�@{33@z��@z~�@zM�@y&�@x�`@x��@x�@xQ�@w�w@w��@wl�@w+@v�y@v��@vff@v{@u�-@u`B@u/@u�@t�@t�j@t��@tj@t(�@sƨ@s�F@s��@s�@sdZ@s33@r�H@r��@rn�@rM�@rM�@rM�@q��@q&�@p��@p��@p�u@o��@o��@o+@n�@n�R@n�R@nv�@n$�@n{@n@n@m@m�h@l��@l��@l��@l�D@lz�@lI�@k�@j�\@i�#@i��@h��@hbN@g�;@g|�@g�@f�@fȴ@f�+@fff@fE�@f$�@e�T@e�-@eO�@eV@d�j@dz�@d9X@d1@c��@c�F@cC�@b��@bM�@a��@ax�@aX@aG�@a&�@`��@`Q�@_�@_+@^��@^v�@^E�@^{@]�-@]p�@]/@\�@\z�@\�@[��@[�m@[��@[C�@Z��@Z-@Y��@ZJ@Y��@X�`@X�9@X �@W��@WK�@Vȴ@Vv�@VV@V$�@U�T@UO�@T�@T�/@Tz�@S��@SdZ@S33@R��@R=q@R�@Q�@Q�7@Q%@PbN@O�@O�P@O�P@O+@N��@Nv�@M��@MV@Lz�@LZ@LI�@K�m@KC�@J�H@J~�@J^5@JJ@I��@Ix�@I7L@H�u@HQ�@G�@G��@G��@G|�@G\)@G;d@G�@F��@F$�@E�@E@E�-@Ep�@E/@D��@D��@Dj@D(�@C��@C�@Ct�@CdZ@B�@B��@B�\@B~�@B^5@B-@BJ@A��@A��@Ax�@Ahs@AX@A7L@A%@@Ĝ@@�@@b@?�w@?l�@?;d@?+@?�@>�R@>�+@>$�@=`B@=V@<�D@<9X@<�@;�
@;t�@;"�@:�!@:M�@:-@9�@9��@9x�@9hs@9&�@8Ĝ@8Q�@8b@7��@7��@7;d@6�y@6v�@5�@5��@5�-@5�@5�@5/@5V@4�@4��@4�D@4z�@4I�@49X@4(�@4�@41@3��@3C�@3"�@2�@2��@2M�@1�#@1��@1��@1x�@17L@1&�@0��@0Ĝ@0r�@0Q�@0b@/��@/��@/|�@.�@.v�@.5?@.@-�@-��@-�-@-�-@-�@-`B@-/@,�/@,Z@,1@+�m@+��@+t�@*�@*n�@*J@)��@)x�@)&�@)�@)%@(��@(Ĝ@(�u@(r�@(bN@(1'@(b@'��@'|�@'K�@'+@'
=@&v�@&@%��@%��@%`B@%?}@%�@$�/@$�j@$�@$�D@$j@$I�@$�@#�m@#��@#S�@#o@"��@"��@"~�@"n�@"^5@"�@"J@!��@!�#@!��@!�^@!x�@!7L@!%@ �`@ r�@ 1'@ b@�@�P@\)@�@�+@v�@ff@V@$�@�h@O�@?}@/@/@�@�@V@��@�/@�j@��@z�@I�@9X@9X@9X@�@�
@��@��@��@�@�@C�@o@��@�!@�!@�!@�!@��@�!@�!@��@��@n�@=q@��@�#@�^@��@7L@��@�9@�u@A�@ �@  @�;@��@��@|�@|�@+@�y@�@�@ȴ@ȴ@�R@��@�+@v�@V@E�@5?@$�@@��@`B@O�@O�@?}@V@�/@��@�j@Z@(�@�
@��@t�@S�@C�@o@�H@�\@^5@-@J@�#@��@��@�7@7L@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�C�A�E�A�G�A�I�A�I�A�I�A�I�A�I�A�C�A�5?A�-A��;A��TA��A�K�A�|�A߲-A�1A޲-A�9XAݙ�A�=qA�Q�A���A׏\A�p�A�x�A�C�A�G�A�33A�9XA�$�A�VA΋DA�{A˴9Aɟ�A�hsA�~�A�1'AčPA�I�A�Q�A�A��A���A�5?A�\)A���A� �A��7A�&�A�hsA���A�7LA�/A�G�A��A���A�(�A�z�A�&�A�ĜA�M�A�A�l�A�M�A�
=A�?}A�I�A��wA�r�A�
=A��#A��DA���A�$�A�A���A��A���A��7A�\)A�bNA�;dA�JA�S�A�(�A�Q�A�7LA��mA��A��A�=qA���A�bNA��A��A�ĜA��-A���A���A��TA���A���A��yA���A�v�A�ZA��A���A�%A~1A{�;Ay�hAxȴAw7LAv1AsƨAq�wApr�AoAn��AmƨAk�PAi��AiK�AiAh�HAh~�AghsAf��AfVAdr�Ac�Aa`BA_oA]�A[�AZ^5AY�TAYVAW`BASAP~�AO/AN�uAMAK��AF�DAE��AEXAE/ADȴAA�A?�#A>��A=33A<�\A<(�A;�
A;�PA:1A8�+A65?A4��A41'A4A3S�A2��A2ZA1oA0I�A/��A.��A-�A-oA,��A*��A*jA)�A(5?A'dZA& �A#�;A!�A�A�uAXAI�A�/A��Az�A�A�jA�FAA�HA�!AA�At�AA1'A"�A$�A\)AbNA�#A��Ax�A;dA�9A�A
5?A	p�A�`At�A��A�\A-A�hA?}A`BAx�A1'A�A��A1Ap�@�\)@��@���@�z�@��@�7@�%@���@�S�@��@��@�$�@�@�9X@�@�b@�-@���@�O�@�S�@��@�K�@�Z@�`B@�u@�\@�ff@�$�@�n�@��@�!@���@��m@�+@ޟ�@ޟ�@�5?@ݡ�@ܣ�@ۮ@ڏ\@ٲ-@�b@�@���@�Z@���@ͩ�@�G�@��@��@�S�@�@�z�@ǅ@���@�v�@Ų-@�%@ļj@�1@�|�@�C�@��@°!@�~�@���@�?}@��`@�z�@�=q@�@��^@���@�1@�1@��m@�|�@��y@��@��`@���@�z�@�9X@���@�@�v�@��/@��m@��;@��F@��P@�|�@�l�@�\)@�K�@�+@��H@��+@�{@���@���@���@�-@�M�@�M�@�`B@��u@�A�@�  @��@���@�ff@��h@��@��9@�r�@���@���@�\)@�;d@���@���@��\@�5?@���@�x�@�X@��@�bN@�1@��
@�|�@�"�@��R@���@�~�@��T@�`B@��@��/@��j@��@��;@��@�dZ@�;d@�+@�"�@�
=@�@���@��@���@�V@�M�@�5?@��@��7@�j@�Q�@�Q�@�I�@�I�@�A�@� �@��m@��w@��F@���@��P@�K�@�o@��@��!@�ff@�=q@�@���@�O�@��@���@�Ĝ@��9@��u@�r�@�9X@�A�@�1@��F@�l�@�C�@�+@���@��!@�{@���@�`B@��@�V@��`@���@���@��u@�A�@��F@���@�t�@�;d@�o@��@��H@��H@��H@��@��\@�ff@�=q@���@�hs@�&�@��/@��u@�j@��@��m@��@�S�@�;d@�@��!@�=q@�J@���@��@�@��@�G�@�&�@���@���@��u@���@�j@��@�t�@�K�@�+@�
=@��@���@�E�@��-@��@�hs@�G�@��/@�1'@�  @�ƨ@���@�dZ@�"�@��H@���@�=q@���@���@��7@��@��@�r�@�A�@�(�@��@���@��F@���@�dZ@�+@��@�ff@�$�@��@�X@�G�@��@��`@��j@��u@���@���@�dZ@�"�@��@���@�@��h@�p�@�hs@�X@�/@��j@�bN@�A�@�b@�@�P@~�y@~5?@}�-@}O�@}V@|�/@|�D@|j@|9X@{��@{�F@{�@{33@z��@z~�@zM�@y&�@x�`@x��@x�@xQ�@w�w@w��@wl�@w+@v�y@v��@vff@v{@u�-@u`B@u/@u�@t�@t�j@t��@tj@t(�@sƨ@s�F@s��@s�@sdZ@s33@r�H@r��@rn�@rM�@rM�@rM�@q��@q&�@p��@p��@p�u@o��@o��@o+@n�@n�R@n�R@nv�@n$�@n{@n@n@m@m�h@l��@l��@l��@l�D@lz�@lI�@k�@j�\@i�#@i��@h��@hbN@g�;@g|�@g�@f�@fȴ@f�+@fff@fE�@f$�@e�T@e�-@eO�@eV@d�j@dz�@d9X@d1@c��@c�F@cC�@b��@bM�@a��@ax�@aX@aG�@a&�@`��@`Q�@_�@_+@^��@^v�@^E�@^{@]�-@]p�@]/@\�@\z�@\�@[��@[�m@[��@[C�@Z��@Z-@Y��@ZJ@Y��@X�`@X�9@X �@W��@WK�@Vȴ@Vv�@VV@V$�@U�T@UO�@T�@T�/@Tz�@S��@SdZ@S33@R��@R=q@R�@Q�@Q�7@Q%@PbN@O�@O�P@O�P@O+@N��@Nv�@M��@MV@Lz�@LZ@LI�@K�m@KC�@J�H@J~�@J^5@JJ@I��@Ix�@I7L@H�u@HQ�@G�@G��@G��@G|�@G\)@G;d@G�@F��@F$�@E�@E@E�-@Ep�@E/@D��@D��@Dj@D(�@C��@C�@Ct�@CdZ@B�@B��@B�\@B~�@B^5@B-@BJ@A��@A��@Ax�@Ahs@AX@A7L@A%@@Ĝ@@�@@b@?�w@?l�@?;d@?+@?�@>�R@>�+@>$�@=`B@=V@<�D@<9X@<�@;�
@;t�@;"�@:�!@:M�@:-@9�@9��@9x�@9hs@9&�@8Ĝ@8Q�@8b@7��@7��@7;d@6�y@6v�@5�@5��@5�-@5�@5�@5/@5V@4�@4��@4�D@4z�@4I�@49X@4(�@4�@41@3��@3C�@3"�@2�@2��@2M�@1�#@1��@1��@1x�@17L@1&�@0��@0Ĝ@0r�@0Q�@0b@/��@/��@/|�@.�@.v�@.5?@.@-�@-��@-�-@-�-@-�@-`B@-/@,�/@,Z@,1@+�m@+��@+t�@*�@*n�@*J@)��@)x�@)&�@)�@)%@(��@(Ĝ@(�u@(r�@(bN@(1'@(b@'��@'|�@'K�@'+@'
=@&v�@&@%��@%��@%`B@%?}@%�@$�/@$�j@$�@$�D@$j@$I�@$�@#�m@#��@#S�@#o@"��@"��@"~�@"n�@"^5@"�@"J@!��@!�#@!��@!�^@!x�@!7L@!%@ �`@ r�@ 1'@ b@�@�P@\)@�@�+@v�@ff@V@$�@�h@O�@?}@/@/@�@�@V@��@�/@�j@��@z�@I�@9X@9X@9X@�@�
@��@��@��@�@�@C�@o@��@�!@�!@�!@�!@��@�!@�!@��@��@n�@=q@��@�#@�^@��@7L@��@�9@�u@A�@ �@  @�;@��@��@|�@|�@+@�y@�@�@ȴ@ȴ@�R@��@�+@v�@V@E�@5?@$�@@��@`B@O�@O�@?}@V@�/@��@�j@Z@(�@�
@��@t�@S�@C�@o@�H@�\@^5@-@J@�#@��@��@�7@7L@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ǮB
ǮB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
ƨB
ĜB
ÖB
�dB
�B
�)B%B�B#�B33B=qB@�BR�B[#BbNBdZB�1B��B��B�3B�wB�9B��B�)B�/B��B��B��B�^B�HB�yB�B�BDB�B#�B�B!�B49B,B&�B,B:^BF�BVBiyBe`BbNBdZB_;B\)BI�BQ�B^5BbNBbNBdZB_;BW
BT�BYBT�BYBZB[#BhsBdZBZBN�BE�B:^B/B �B�B�B�B�B�B�BoB��B�B��B�wB�B��B��B��B{�BbNBL�B8RB#�B1B
��B
�B
��B
��B
bNB
VB
bNB
XB
A�B
�B
1B
B
  B
1B
	7B
B	��B	�`B	�;B	�B	��B	ĜB	�3B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	|�B	q�B	e`B	W
B	P�B	I�B	:^B	"�B��B��B��B�B�#B�XB��B��B��BƨB�B�'B�9B�B�3B�9B�'B�B��B��B��B�oB��B��B��B��B��B�{B�uB�uB�\B�DB�bB�{B�JB��B�{B�bB�oB�1B~�Bw�Bq�Br�Bm�BiyBdZBe`BgmBgmBhsBhsBo�Bu�Br�Bn�Bl�Bo�BjBk�Bl�Bo�Bn�Bq�Bs�Br�Bn�BhsB_;BbNBdZBcTB^5Bp�Br�Bt�By�B�%B�JB�{B�bB�oB��B��B��B�uB�=B~�B�B�B~�B�B�B�1B��B��B��B��B��B�uB�\B�\B��B��B�{B��B��B��B�B�B��B��B�B�'B�^B�qB�qB�RB�dB�jB�wB��B�}B�jB�RB�?B�'B��B��B��B�\B�VB�hB�{B�bB��B��B��B��B��B��B�B��B��B�B�'B�9B�XB�^B�dB�qB�qB��BĜBŢBB��B��B��B��B�B�)B�B�B�
B�;B�sB�yB�B�B�B�B�B�B��B	  B	B	+B	+B	1B	JB	VB	bB	hB	oB	�B	�B	�B	"�B	%�B	'�B	(�B	.B	5?B	8RB	9XB	:^B	=qB	?}B	E�B	H�B	K�B	L�B	N�B	Q�B	R�B	T�B	W
B	XB	ZB	]/B	^5B	bNB	cTB	cTB	iyB	l�B	l�B	n�B	p�B	s�B	s�B	q�B	v�B	z�B	}�B	~�B	~�B	~�B	�B	�%B	�1B	�=B	�=B	�=B	�DB	�DB	�DB	�=B	�JB	�\B	�\B	�\B	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�FB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�wB	��B	��B	��B	B	B	ĜB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�
B	�B	�
B	�
B	�
B	�#B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B

=B
	7B
+B
1B
1B
	7B
	7B

=B
	7B
JB
\B
bB
\B
VB
PB
VB
hB
hB
oB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
$�B
%�B
%�B
$�B
#�B
"�B
"�B
$�B
&�B
%�B
&�B
'�B
(�B
)�B
+B
,B
,B
,B
-B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
-B
,B
,B
-B
-B
-B
.B
.B
.B
-B
-B
-B
-B
.B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
2-B
2-B
2-B
2-B
49B
5?B
49B
33B
5?B
49B
5?B
5?B
5?B
6FB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
7LB
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
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
?}B
?}B
>wB
=qB
?}B
?}B
A�B
@�B
@�B
@�B
A�B
@�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
K�B
J�B
L�B
L�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
Q�B
Q�B
P�B
P�B
Q�B
Q�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
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
W
B
XB
XB
W
B
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
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
^5B
_;B
_;B
_;B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
e`B
ffB
ffB
e`B
ffB
ffB
e`B
gmB
gmB
gmB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
iyB
jB
k�B
k�B
k�B
k�B
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
p�B
p�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
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
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ǮB
ǮB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
��B
��B
��B
��B
��B
��BmBKB#�B4TB>BBA�BT�B]�Bd�Bh$B��B�]B�HB��B��B�+B��B�xB�OB�9B�B��B��B��B��B��B�3B�B�B%FB�B$�B5tB-�B(�B-CB;�BG�BW�Bj�BgBd�Bf�Ba�B^�BM6BS@B^�Bc:BcTBe`B`�BY�BW�BZ�BV�BZ7BZ�B\BiBe`B[�BP�BHKB=B1vB#�BOB�B_B/B�B�B�B;B�B��B� B�-B��B��B��B�BffBPB;�B(�B�B
��B
��B
�,B
��B
gB
YB
b�B
Y�B
EB
 �B

�B
�B
uB
	lB
B
�B	�fB	�B	�B	�B	� B	�tB	��B	��B	��B	�_B	�RB	��B	��B	�xB	��B	��B	��B	��B	�B	s�B	g�B	X�B	Q�B	K^B	="B	'B	�B	 �B�B�[B�jB�wBѷBуB�vB�B��B��B��B��B�B��B��B�B��B��B�+B�aB�BB�bB��B�VB��B�B��B�{B��B��B�4B��B�pB�B�B��B��B�XB��Bz�BtTBt9BoOBkBfLBgBh�Bh�Bi�Bi�Bp�Bu�Bs3Bo�Bm�BpoBk�Bl�Bm�Bp�Bo�BraBtBsBo5Bi�Ba-Bc�BezBdZB`'Bq[BsMButBz�B��B�~B��B� B�uB�#B��B�'B�B�B��B�{B�B� B��B��B��B�gB��B��B�#B��B��B�4B��B��B�WB��B��B��B�sB��B��B�B�*B�)B�B�^B��B�BB��B��B��B��B��B�B�<B�$B�B��B�QB��B�pB�TB��B��B��B��B��B��B��B��B�dB�ZB�WB��B�yB�]B��B��B��B��B��B��B�B��B�B�?B��B�<B�BЗB҉B�QB�]BچBخB�BߤB�B��B��B��B�;B�GB�B�TB�B	 4B	3B	EB	_B	KB	dB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	($B	)�B	.�B	5tB	8�B	9�B	:�B	=�B	@4B	FB	IB	LB	MPB	O(B	R:B	S&B	U2B	W?B	XEB	Z�B	]dB	^�B	b�B	c�B	c�B	i�B	l�B	l�B	n�B	p�B	s�B	tB	r-B	wB	{0B	~(B	.B	HB	}B	�9B	�YB	�fB	�XB	�XB	�XB	�^B	�^B	�^B	��B	�~B	�vB	�vB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�>B	�*B	�6B	�6B	�]B	�iB	�|B	�`B	�fB	�fB	�fB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�"B	�BB	�,B	�,B	�,B	�B	�$B	�+B	�B	�+B	�$B	�9B	�?B	�?B	�sB	�WB	�WB	�xB	�jB	�pB	ߊB	��B	�B	�B	�tB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�-B	��B	��B	��B	��B	��B	��B	�B	�9B	��B	��B	�B	�FB	�FB	�B	�B	�B	�B	�B	�B	�6B	�VB	�VB	�.B	�.B	�]B	�BB
 4B
 B
 B
;B
 B
'B
'B
;B
[B
{B
{B
SB
�B
?B
fB
	RB
	lB

XB
	lB
�B
fB
�B
	lB
	lB

�B
	�B
�B
�B
}B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
�B
 �B
 �B
 �B
 B
!�B
!�B
"�B
#�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$B
$�B
%�B
%�B
$�B
$&B
# B
#:B
%B
'B
&2B
'B
($B
)DB
*0B
+6B
,"B
,=B
,"B
-B
,"B
,"B
,"B
,=B
-)B
-)B
./B
./B
./B
./B
-)B
,=B
,=B
-CB
-)B
-CB
./B
./B
./B
-CB
-CB
-CB
-]B
.IB
/5B
0UB
0UB
0;B
0;B
1[B
1[B
1[B
2GB
2GB
3hB
2GB
2aB
2|B
2aB
4TB
5ZB
4nB
3�B
5ZB
4�B
5tB
5tB
5�B
6`B
7�B
7fB
8�B
7�B
8lB
8�B
8�B
7�B
9rB
9rB
9�B
9�B
:xB
:xB
:�B
:�B
:�B
:�B
<jB
<�B
<�B
<�B
;�B
;�B
<�B
=�B
?�B
?�B
>�B
=�B
?�B
?�B
A�B
@�B
@�B
@�B
A�B
@�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
LB
KB
MB
MB
M�B
N�B
N�B
NB
N�B
OB
N�B
O�B
O�B
O�B
QB
Q B
PB
PB
P.B
QB
RB
RB
QB
Q4B
R B
R B
TB
TB
T,B
TB
TB
TB
TB
T,B
T�B
UB
UB
T�B
T�B
UB
UB
T,B
UB
U2B
V9B
U2B
VB
VB
W$B
W$B
W$B
W$B
W$B
W$B
W$B
W$B
X+B
X+B
W$B
X+B
X+B
WYB
XEB
YKB
YKB
Y1B
YKB
ZB
ZB
Z7B
Z7B
ZQB
YKB
ZQB
[WB
[=B
[WB
[=B
[WB
[WB
\]B
\CB
\CB
]IB
^5B
^5B
^OB
^OB
^OB
^OB
_;B
_pB
^OB
_VB
^OB
_VB
_VB
_VB
^jB
_�B
`\B
`\B
`\B
abB
abB
a|B
bhB
bhB
bhB
bhB
b�B
bhB
bhB
bhB
b�B
b�B
cnB
c�B
cnB
dZB
d�B
dtB
dZB
dtB
dtB
dZB
dtB
d�B
dtB
ezB
ezB
d�B
ezB
f�B
f�B
e�B
f�B
f�B
e�B
gmB
g�B
g�B
g�B
f�B
h�B
i�B
iyB
iyB
iyB
iyB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
jB
jB
jB
j�B
i�B
j�B
k�B
k�B
k�B
k�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
p�B
p�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
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
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709260033372017092600333720170926003337201806221319312018062213193120180622131931201804050722012018040507220120180405072201  JA  ARFMdecpA19c                                                                20170922093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170922003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170922003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170922003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170922003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170922003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170922003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170922003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170922003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170922003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20170922005549                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170924155806  CV  JULD            G�O�G�O�F�?�                JM  ARCAJMQC2.0                                                                 20170925153337  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170925153337  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222201  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041931  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                