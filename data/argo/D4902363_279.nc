CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-11T00:35:14Z creation;2018-09-11T00:35:19Z conversion to V3.1;2019-12-19T07:33:01Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180911003514  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_279                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؀tN� 1   @؀t��� @9�&��IR�dU�>BZ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
=@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BOp�BW�
B_�
Bg�
Bo�
Bx=qB�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C0\C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D��D�>�D�~�DǾ�D���D�>�D�{�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��`A��/A���A���A���A���A�ȴA���A�ƨA�Aհ!AլAլAծAծAՕ�AՅA�z�A�l�A�ZA�C�A�A�A�?}A��A���A҅A�VA�z�A�r�AŬA� �A�ȴA�VA��!A�{A��A�VA�ƨA�A�A���A��yA��`A��A�bA�-A�A�dZA�+A��A���A�bA��-A�A�A�K�A��^A�G�A���A���A�^5A�
=A�\)A�K�A��A��/A�ȴA�x�A���A���A��uA�\)A��FA�33A��A���A��A��A�A�A�x�A��A���A�S�A�  A��A���A�z�A��wA��\A�hsA�{A��A���A�7LA��^A�33A�x�A��yA��A�;dA���A�l�A�=qA�+A�jA��A���A�VA�G�A��^A�A~ĜA~1A}l�A|�AzVAw&�Au��As+AqK�Ap�Ao�TAo|�Ao�Am�Ajz�Ah�/Ahn�Ag�Ad��Ac��Ab�AbbAa`BA`�9A_��A_;dA^jA]x�A\$�AZĜAZ1'AY�;AY�AYp�AYK�AXZAW�AV{AT1'AR-APVAN��AL�9AKƨAK�AJ��AJ5?AHĜAHr�AGx�AF��AE�AE��AD��AD  ADbACO�ABAA�A>�A=hsA<�A;�PA:ĜA:  A9hsA8��A8��A8A7�PA7O�A6^5A5�;A5%A4r�A3�7A29XA1��A1�A0Q�A.��A-��A,I�A+��A*jA)G�A(��A&��A%��A%&�A$z�A#x�A"v�A"jA"ZA"-A!�wA!oA z�A��A5?A��AbA�-AK�A��A��A�+A�FAjAt�AffA�7A�/A�mA�-Al�A��AI�A�TA��AG�AffAl�A/A��A
=qA	�-A	�A	dZA	VAȴA��A��A�\An�AQ�AA��A��A;dA��A-A��A��A ��A -@�n�@�G�@���@���@�t�@��+@�p�@�A�@�dZ@��@��@�7@��@�j@�~�@��@�h@�p�@��@蛦@�  @�$�@���@���@��#@���@���@�C�@ޏ\@���@ݡ�@۝�@��@���@�=q@�?}@�1'@���@� �@��@���@�M�@́@̛�@˥�@��y@ɡ�@ț�@�33@�1'@�+@�@�V@���@�z�@�Z@�b@�dZ@���@�E�@���@���@� �@��m@��F@�\)@�@�ff@��#@��@�A�@�t�@��#@��@�bN@���@�
=@���@�^5@��@�I�@�v�@��h@��@�1'@��@�+@�@��@�C�@��y@�$�@���@��h@���@�A�@�(�@��@��;@���@�C�@�~�@�X@���@�9X@�  @��w@�C�@���@�5?@���@��-@��-@���@��@�%@��9@�b@���@�dZ@�K�@��H@��@�%@�9X@�33@�=q@�V@�Ĝ@�(�@��@��@�|�@�o@��+@��#@�X@�Ĝ@��
@�dZ@�o@���@�V@��@��7@�j@��
@�@�ȴ@�v�@�-@��T@��7@�X@�/@��`@�bN@���@��@��P@�K�@�
=@��!@�^5@���@�@��-@��h@�V@��@�Ĝ@�r�@�(�@��;@���@�S�@��@���@��+@�{@�@���@��@�`B@�%@��@�I�@��@��@|�@K�@+@~ȴ@~V@~{@~{@~5?@~$�@~{@}�@}@}��@}p�@}O�@}?}@|�@|Z@|�@{�m@{�F@{��@{��@{t�@z��@z��@zM�@y��@y%@x��@xr�@w�@wK�@vȴ@v��@u��@u�@up�@t��@t�@t��@t�D@tZ@t9X@s�
@sƨ@s�@r��@rn�@r=q@q�#@q7L@q�@q%@p��@o;d@n�y@nv�@m��@l�/@lI�@l9X@k��@k"�@j�@j^5@i��@i�@i%@h�`@h��@hA�@hb@h  @gK�@g�@g+@gK�@fȴ@f��@fv�@f{@e��@e�-@e�h@ep�@eO�@e�@d�@d�D@dZ@dI�@c��@cƨ@c��@ct�@cS�@cC�@cS�@c@b��@bn�@bn�@bn�@bJ@a��@ax�@ahs@ahs@a�@`Ĝ@`�u@`��@`�u@`r�@`Q�@`b@_�;@_��@_��@_�w@_�@_|�@_+@^�R@^��@^�+@^V@]@]p�@]/@]V@\�j@\z�@\9X@\(�@[��@[ƨ@[��@[��@[��@[��@[��@Z�!@Y��@Y7L@Y�@X��@X��@X�9@X�9@X��@XbN@XQ�@X �@W��@W�w@W�P@Wl�@WK�@W+@V�R@VV@U��@U�@Up�@T�/@Tz�@T�@S��@So@R��@R��@Q��@Q�7@Qhs@QX@Q&�@P��@PĜ@PbN@O�;@O|�@O+@N��@Nȴ@Nȴ@N�R@N��@NV@N@M��@M?}@L�/@Lz�@L�@K�F@KdZ@Ko@Jn�@J-@I�@I��@Ix�@I7L@H�9@H �@G�@G��@F��@F�@Fȴ@F�R@F��@Fv�@E��@Ep�@E�@D��@D�D@Dj@D�@C�F@CC�@Co@B��@B^5@B-@A��@A�@A�#@A��@Ax�@@��@@��@@�@@1'@@  @?�;@?��@?;d@>�y@>��@>�+@>ff@>5?@=��@=�@=?}@<�@<��@<I�@;��@;�
@;t�@;o@:�!@:n�@9��@9x�@97L@8Ĝ@8Q�@8 �@7��@7;d@7�@6�y@6��@6$�@5��@5�-@5�@5`B@5/@4��@4�D@4j@4I�@3��@3�
@3�
@3�
@3�F@3��@3S�@2��@2-@1��@1�#@1�^@1x�@1&�@1�@0�`@0��@0r�@0bN@0A�@/�@/�w@/;d@.�R@.��@.v�@.$�@-��@-`B@-�@,�@,�@,�/@,�/@,��@,Z@,�@+��@+ƨ@+�@+S�@+"�@+@*��@*��@*n�@*�@)��@)x�@)G�@)%@(��@(�@( �@'��@'|�@'\)@&ȴ@&{@%��@%�-@%�@%`B@%O�@%/@%V@$��@$�@$�/@$�j@$��@$�D@$z�@$j@$I�@$(�@$�@$1@#�m@#�
@#�F@#��@#dZ@#C�@#o@"�H@"~�@"-@!�@!�#@!�^@!hs@!&�@!%@ ��@ ��@ Ĝ@ ��@ �@ bN@ A�@ A�@  �@�;@�w@�P@\)@;d@�y@�R@��@��@�+@�+@ff@{@��@`B@��@�j@��@�@�m@�m@��@C�@o@o@@�\@M�@=q@�@��@x�@x�@hs@hs@��@�u@bN@ �@  @�P@K�@;d@�y@ff@V@$�@@�-@�@�@�j@�D@I�@�@1@��@��@33@"�@��@��@�\@~�@n�@^5@J@�^@��@7L@��@��@�u@r�@A�@b@�@�P@|�@K�@�@ȴ@��@�+@v�@ff@V@$�@{@{@@�-@�@`B@O�@/@�@�@�/@�@z�@I�@(�@�
@�@t�@@
�!@
^5@
�@
�@
�@
�@
J@	��@	�^@	7L@��@��@Q�@b@��@�P@l�@\)@+@�@��@�+@v�@V@5?@�T@��@`B@/@�@�j@�D@j@Z@I�@�@��@�F@�F@�F@��@�@o@�H@�H@�H@��@��@~�@M�@=q@J@�@��@hs@&�@ ��@ �@ Q�@ 1'@ b@   @   ?��;?��;?���?�|�?�;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��`A��/A���A���A���A���A�ȴA���A�ƨA�Aհ!AլAլAծAծAՕ�AՅA�z�A�l�A�ZA�C�A�A�A�?}A��A���A҅A�VA�z�A�r�AŬA� �A�ȴA�VA��!A�{A��A�VA�ƨA�A�A���A��yA��`A��A�bA�-A�A�dZA�+A��A���A�bA��-A�A�A�K�A��^A�G�A���A���A�^5A�
=A�\)A�K�A��A��/A�ȴA�x�A���A���A��uA�\)A��FA�33A��A���A��A��A�A�A�x�A��A���A�S�A�  A��A���A�z�A��wA��\A�hsA�{A��A���A�7LA��^A�33A�x�A��yA��A�;dA���A�l�A�=qA�+A�jA��A���A�VA�G�A��^A�A~ĜA~1A}l�A|�AzVAw&�Au��As+AqK�Ap�Ao�TAo|�Ao�Am�Ajz�Ah�/Ahn�Ag�Ad��Ac��Ab�AbbAa`BA`�9A_��A_;dA^jA]x�A\$�AZĜAZ1'AY�;AY�AYp�AYK�AXZAW�AV{AT1'AR-APVAN��AL�9AKƨAK�AJ��AJ5?AHĜAHr�AGx�AF��AE�AE��AD��AD  ADbACO�ABAA�A>�A=hsA<�A;�PA:ĜA:  A9hsA8��A8��A8A7�PA7O�A6^5A5�;A5%A4r�A3�7A29XA1��A1�A0Q�A.��A-��A,I�A+��A*jA)G�A(��A&��A%��A%&�A$z�A#x�A"v�A"jA"ZA"-A!�wA!oA z�A��A5?A��AbA�-AK�A��A��A�+A�FAjAt�AffA�7A�/A�mA�-Al�A��AI�A�TA��AG�AffAl�A/A��A
=qA	�-A	�A	dZA	VAȴA��A��A�\An�AQ�AA��A��A;dA��A-A��A��A ��A -@�n�@�G�@���@���@�t�@��+@�p�@�A�@�dZ@��@��@�7@��@�j@�~�@��@�h@�p�@��@蛦@�  @�$�@���@���@��#@���@���@�C�@ޏ\@���@ݡ�@۝�@��@���@�=q@�?}@�1'@���@� �@��@���@�M�@́@̛�@˥�@��y@ɡ�@ț�@�33@�1'@�+@�@�V@���@�z�@�Z@�b@�dZ@���@�E�@���@���@� �@��m@��F@�\)@�@�ff@��#@��@�A�@�t�@��#@��@�bN@���@�
=@���@�^5@��@�I�@�v�@��h@��@�1'@��@�+@�@��@�C�@��y@�$�@���@��h@���@�A�@�(�@��@��;@���@�C�@�~�@�X@���@�9X@�  @��w@�C�@���@�5?@���@��-@��-@���@��@�%@��9@�b@���@�dZ@�K�@��H@��@�%@�9X@�33@�=q@�V@�Ĝ@�(�@��@��@�|�@�o@��+@��#@�X@�Ĝ@��
@�dZ@�o@���@�V@��@��7@�j@��
@�@�ȴ@�v�@�-@��T@��7@�X@�/@��`@�bN@���@��@��P@�K�@�
=@��!@�^5@���@�@��-@��h@�V@��@�Ĝ@�r�@�(�@��;@���@�S�@��@���@��+@�{@�@���@��@�`B@�%@��@�I�@��@��@|�@K�@+@~ȴ@~V@~{@~{@~5?@~$�@~{@}�@}@}��@}p�@}O�@}?}@|�@|Z@|�@{�m@{�F@{��@{��@{t�@z��@z��@zM�@y��@y%@x��@xr�@w�@wK�@vȴ@v��@u��@u�@up�@t��@t�@t��@t�D@tZ@t9X@s�
@sƨ@s�@r��@rn�@r=q@q�#@q7L@q�@q%@p��@o;d@n�y@nv�@m��@l�/@lI�@l9X@k��@k"�@j�@j^5@i��@i�@i%@h�`@h��@hA�@hb@h  @gK�@g�@g+@gK�@fȴ@f��@fv�@f{@e��@e�-@e�h@ep�@eO�@e�@d�@d�D@dZ@dI�@c��@cƨ@c��@ct�@cS�@cC�@cS�@c@b��@bn�@bn�@bn�@bJ@a��@ax�@ahs@ahs@a�@`Ĝ@`�u@`��@`�u@`r�@`Q�@`b@_�;@_��@_��@_�w@_�@_|�@_+@^�R@^��@^�+@^V@]@]p�@]/@]V@\�j@\z�@\9X@\(�@[��@[ƨ@[��@[��@[��@[��@[��@Z�!@Y��@Y7L@Y�@X��@X��@X�9@X�9@X��@XbN@XQ�@X �@W��@W�w@W�P@Wl�@WK�@W+@V�R@VV@U��@U�@Up�@T�/@Tz�@T�@S��@So@R��@R��@Q��@Q�7@Qhs@QX@Q&�@P��@PĜ@PbN@O�;@O|�@O+@N��@Nȴ@Nȴ@N�R@N��@NV@N@M��@M?}@L�/@Lz�@L�@K�F@KdZ@Ko@Jn�@J-@I�@I��@Ix�@I7L@H�9@H �@G�@G��@F��@F�@Fȴ@F�R@F��@Fv�@E��@Ep�@E�@D��@D�D@Dj@D�@C�F@CC�@Co@B��@B^5@B-@A��@A�@A�#@A��@Ax�@@��@@��@@�@@1'@@  @?�;@?��@?;d@>�y@>��@>�+@>ff@>5?@=��@=�@=?}@<�@<��@<I�@;��@;�
@;t�@;o@:�!@:n�@9��@9x�@97L@8Ĝ@8Q�@8 �@7��@7;d@7�@6�y@6��@6$�@5��@5�-@5�@5`B@5/@4��@4�D@4j@4I�@3��@3�
@3�
@3�
@3�F@3��@3S�@2��@2-@1��@1�#@1�^@1x�@1&�@1�@0�`@0��@0r�@0bN@0A�@/�@/�w@/;d@.�R@.��@.v�@.$�@-��@-`B@-�@,�@,�@,�/@,�/@,��@,Z@,�@+��@+ƨ@+�@+S�@+"�@+@*��@*��@*n�@*�@)��@)x�@)G�@)%@(��@(�@( �@'��@'|�@'\)@&ȴ@&{@%��@%�-@%�@%`B@%O�@%/@%V@$��@$�@$�/@$�j@$��@$�D@$z�@$j@$I�@$(�@$�@$1@#�m@#�
@#�F@#��@#dZ@#C�@#o@"�H@"~�@"-@!�@!�#@!�^@!hs@!&�@!%@ ��@ ��@ Ĝ@ ��@ �@ bN@ A�@ A�@  �@�;@�w@�P@\)@;d@�y@�R@��@��@�+@�+@ff@{@��@`B@��@�j@��@�@�m@�m@��@C�@o@o@@�\@M�@=q@�@��@x�@x�@hs@hs@��@�u@bN@ �@  @�P@K�@;d@�y@ff@V@$�@@�-@�@�@�j@�D@I�@�@1@��@��@33@"�@��@��@�\@~�@n�@^5@J@�^@��@7L@��@��@�u@r�@A�@b@�@�P@|�@K�@�@ȴ@��@�+@v�@ff@V@$�@{@{@@�-@�@`B@O�@/@�@�@�/@�@z�@I�@(�@�
@�@t�@@
�!@
^5@
�@
�@
�@
�@
J@	��@	�^@	7L@��@��@Q�@b@��@�P@l�@\)@+@�@��@�+@v�@V@5?@�T@��@`B@/@�@�j@�D@j@Z@I�@�@��@�F@�F@�F@��@�@o@�H@�H@�H@��@��@~�@M�@=q@J@�@��@hs@&�@ ��@ �@ Q�@ 1'@ b@   @   ?��;?��;?���?�|�?�;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�+B�wB�BĜB%�BB�By�B�%B}�BXB]/BE�B�BVBs�B_;B<jBI�BO�B@�B7LB2-B:^B0!BL�BYBA�BI�B:^B)�BW
BS�BI�BB�B<jB5?B8RB&�BbB  BVB�B�}BƨB�B�B�`B�
B��B��B�9B��B�LB�-B��B��B��B��B��B�VB�7B�B}�B�1B�%Bz�Bm�BdZBXBO�BJ�B49B.BbB
��B
�B
�}B
�B
��B
��B
�hB
�+B
u�B
M�B
@�B
XB
T�B
W
B
N�B
B�B
�B	��B
�B	��B	��B
uB
{B
bB
B	�sB	��B	�#B	�yB	�B	�3B	�dB	�jB	�RB	�'B	�B	��B	��B	�uB	�+B	x�B	p�B	x�B	x�B	t�B	u�B	o�B	[#B	D�B	?}B	�B	�B	
=B	DB��B	bB	JB	oB		7B�B	%B��B��B��B��B��B�B	
=B��B�yB�B��B�HB�)B�yB�NB�HB�TB�ZB�ZB�;B�B�B��B��BB��B�LB�B�3B�-B��B�{B�\B�1B�oB�+B~�B�Bk�B{�B�%B~�Bz�Bw�B�JB�7B�By�Bq�BiyBcTBS�BN�Be`Be`BbNBO�BO�B[#BO�BC�BG�BD�BF�BG�BF�BP�BQ�BI�BE�BG�BH�BB�B8RB/B=qB'�B!�B>wBF�BH�BD�BF�BI�BJ�BJ�BG�BE�B@�B?}B<jB49B+B�B+B��BoB"�B�B$�B/B&�B+B%�B�B�B�BuB%BhBB+BoB�B+B(�B$�B�B�BJB+B�B�B�B�B�B�B�B�B%B  BoBPB�B\B
=BDB�B$�B!�B�B�B�B�B{B�BbBB!�B)�B-B)�B$�B2-B0!B,B'�B0!B,B,B33B7LB7LB5?B49B1'B1'B0!B-B0!B)�B5?B9XB;dB>wBA�B?}B9XB6FB6FBB�BJ�BJ�BN�BK�BH�BH�BL�B[#B[#B_;BbNB_;BbNBjBk�BhsBhsBffBdZBcTBl�Bq�Bv�Bv�Bu�Bt�Bx�B}�B�B�B�B�B}�B� B� B�B�DB�=B�B� B�B�7B�DB��B��B��B��B�!B�'B�3B�'B�-B�9B�RB�XB�dBŢB��B��B��B��B��BǮB��B�B�HB�NB�ZB�fB�sB�B�B�B�B�B��B��B��B��B��B��B	  B	B	1B	+B	B	VB	VB	VB	hB	�B	�B	�B	�B	�B	�B	�B	$�B	+B	,B	-B	-B	1'B	9XB	<jB	>wB	C�B	C�B	D�B	D�B	E�B	G�B	J�B	K�B	L�B	L�B	L�B	M�B	N�B	N�B	O�B	P�B	O�B	P�B	VB	XB	XB	ZB	ZB	YB	XB	]/B	]/B	]/B	bNB	gmB	gmB	gmB	hsB	jB	n�B	m�B	t�B	v�B	v�B	y�B	{�B	}�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�1B	�=B	�bB	�bB	�VB	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�-B	�?B	�LB	�FB	�dB	�qB	�}B	�qB	�wB	�}B	�wB	��B	B	B	ÖB	ĜB	ÖB	ŢB	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�)B	�B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�`B	�`B	�`B	�ZB	�TB	�;B	�HB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B
1B

=B
	7B
	7B

=B
JB
JB
JB
\B
bB
bB
\B
\B
VB
\B
hB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
&�B
%�B
&�B
(�B
(�B
'�B
(�B
)�B
+B
+B
,B
,B
,B
+B
-B
-B
-B
.B
/B
.B
.B
.B
,B
,B
-B
0!B
1'B
0!B
0!B
0!B
2-B
1'B
1'B
2-B
33B
2-B
2-B
2-B
1'B
2-B
49B
49B
49B
49B
5?B
6FB
7LB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
?}B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
E�B
F�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
M�B
L�B
M�B
N�B
M�B
M�B
N�B
O�B
N�B
M�B
N�B
O�B
N�B
O�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
S�B
R�B
Q�B
S�B
S�B
S�B
R�B
Q�B
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
W
B
YB
YB
YB
YB
YB
XB
XB
YB
XB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
\)B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
`BB
aHB
aHB
`BB
aHB
bNB
aHB
bNB
cTB
dZB
e`B
e`B
e`B
e`B
dZB
cTB
cTB
dZB
e`B
e`B
ffB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
jB
iyB
iyB
iyB
iyB
jB
jB
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
o�B
o�B
n�B
n�B
m�B
o�B
p�B
p�B
p�B
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
p�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�ZB��B)�BE�Bz�B�_B��B]IBa|BL0B'RBYBu%BbNB@�BK�BQ�BB�B9rB4�B<B2�BMPBYeBC{BKxB>�B.IBW�BUMBJ�BDB=�B6�B9XB(�B�B�B�B�BĜBɠB�B�B�BؓB�B��B�zB�B�B�MB��B��B��B��B��B��B�rB�YBcB��B��B{�Bo Be�BY�BQ4BK�B6B/�B�B
�PB
�]B
��B
��B
��B
��B
��B
�B
xB
R:B
C�B
Y�B
VSB
W�B
O�B
C�B
!�B	��B
�B	�B
 �B
FB
2B
 B
YB	�B	�B	��B	�0B	��B	��B	��B	�VB	�XB	�-B	��B	��B	��B	��B	��B	z�B	rGB	yrB	y>B	u?B	u�B	p!B	\�B	FtB	@�B	jB	B	�B	6B��B	NB	B	�B	
#B��B	�B�B��B��B��B��B��B	
#B�B�kB�B��B��B��B�eB�B�NB�&B��B��B�B��BڠB��B˒B��B�oB��B��B�B��B�2B�SB� B�#B�uB��B��B�%Bn/B}"B�B�B|PBy$B�dB�lB�{Bz�Br�Bj�Bd�BVBP�BfBfBc BQ�BQNB\BQ4BEmBIBF%BG�BH�BG�BQNBRoBJ�BF�BHKBI7BCaB9�B0�B>(B)�B#�B>�BGBIBEBGBI�BJ�BKBG�BE�BAB?�B<�B4�B+�B�B	�B�	B�B#�B �B%�B/�B'�B+kB&�B �B �BxB�B�BTB�B�BuB BB+B)*B%FB BB7B�B�BeBWB_BkB;BIB#B
B�B�B&B�B9BHB�B�BIB%,B"NB]B]BeBQB�BmB�BB"�B*B-]B*�B%�B2aB0�B,�B(�B0�B,�B,�B3�B7�B7�B5�B4�B1�B1�B0�B-�B0�B+B5�B9�B<B?BA�B@ B:*B7LB7�BCGBKDBK^BO\BLdBI�BI�BM�B[qB[�B_�Bb�B_�Bb�Bj�Bk�Bh�Bh�Bf�BeBdZBmBq�Bv�BwBv+Bu?ByXB~BB�-B�3B�aB�;B~wB�iB��B��B�xB�rB��B��B��B�	B�JB�9B�kB�$B��B�UB�vB�hB��B��B��B��B��B�6B��B�B�DB�B�<B�xBȀB�[BּB�|B�B�B��B��B�B��B��B�B��B��B�$B�$B�B�0B�]B	 OB	mB	KB	zB	�B	pB	�B	�B	�B	�B	�B	�B	�B	B	B	 'B	%,B	+B	,=B	-CB	-]B	1�B	9�B	<�B	>�B	C�B	C�B	D�B	D�B	E�B	G�B	J�B	K�B	L�B	MB	L�B	M�B	N�B	N�B	O�B	Q B	P.B	QNB	VB	XEB	X+B	Z7B	Z7B	Y1B	XyB	]IB	]~B	]~B	b�B	g�B	g�B	g�B	h�B	j�B	n�B	m�B	t�B	v�B	wB	y�B	|B	~B	�4B	�B	�;B	�-B	�aB	�[B	�?B	�fB	��B	�rB	�bB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�B	�B	�2B	�RB	�6B	�;B	�;B	�UB	�aB	�ZB	�fB	��B	�dB	�qB	�}B	��B	��B	��B	��B	��B	ªB	ªB	ðB	��B	ðB	żB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�&B	��B	�,B	� B	� B	�,B	��B	�B	�B	�B	�?B	�+B	�+B	�$B	�$B	�+B	�1B	�1B	�B	�7B	�7B	�7B	�7B	�QB	�CB	�dB	�CB	�kB	�OB	�VB	�\B	�vB	�|B	�hB	�nB	�B	�B	�zB	�`B	�`B	�tB	�nB	ߤB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�	B	��B	�B	�B	�B	�B	�6B	�B	�"B	�B
 B
 B
B
 B
 B
 4B
 B
;B
[B
aB
gB
MB
SB
YB
YB
tB
KB
	lB
fB

rB
	lB
	�B

rB
dB
~B
�B
\B
}B
}B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
# B
# B
%B
%B
%B
'B
&B
'B
)B
)B
($B
)DB
*B
+6B
+6B
,"B
,=B
,"B
+QB
-)B
-)B
-)B
.IB
/5B
.B
./B
./B
,=B
,WB
-CB
0UB
1AB
0UB
0;B
0UB
2GB
1AB
1AB
2aB
33B
2GB
2GB
2aB
1[B
2aB
4TB
4nB
4nB
4nB
5tB
6`B
7fB
8RB
8RB
7�B
7fB
7fB
8lB
8lB
8lB
8lB
9�B
9�B
9�B
:�B
9rB
9�B
9�B
9rB
:xB
;�B
;B
;B
<�B
<�B
<�B
<�B
=�B
<�B
=�B
?�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
E�B
F�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
KB
J�B
K�B
K�B
K�B
M�B
MB
NB
N�B
NB
M�B
N�B
O�B
N�B
N"B
N�B
O�B
OB
O�B
Q�B
Q�B
Q�B
Q B
PB
Q B
RB
RB
RB
R B
RB
T,B
SB
R B
S�B
T,B
T,B
S&B
R B
UB
UB
VB
VB
VB
W
B
W$B
V9B
VSB
X+B
W$B
Y1B
YB
YB
YB
YKB
X+B
XEB
Y1B
X_B
Z7B
[WB
[=B
[WB
[=B
[=B
[=B
\CB
]/B
\CB
]dB
]dB
^OB
_;B
_;B
_;B
_VB
_VB
_;B
`\B
_pB
^OB
_pB
`BB
`BB
`\B
`vB
aHB
aHB
`\B
`\B
abB
abB
`\B
a|B
bhB
a�B
bhB
cnB
dtB
e`B
e`B
ezB
e`B
dtB
c�B
c�B
d�B
ezB
e�B
f�B
g�B
g�B
h�B
h�B
h�B
g�B
h�B
j�B
i�B
i�B
i�B
i�B
j�B
j�B
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
o�B
o�B
n�B
n�B
m�B
o�B
p�B
p�B
p�B
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
p�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809150035342018091500353420180915003534201809150200162018091502001620180915020016201809160021382018091600213820180916002138  JA  ARFMdecpA19c                                                                20180911093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180911003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180911003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180911003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180911003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180911003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180911003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180911003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180911003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180911003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180911005602                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180911153608  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180911153608  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180911153608  CV  LATITUDE        G�O�G�O�A̍P                JM  ARGQJMQC2.0                                                                 20180911153608  CV  LONGITUDE       G�O�G�O��"�V                JM  ARCAJMQC2.0                                                                 20180914153534  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180914153534  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180914170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180915152138  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                