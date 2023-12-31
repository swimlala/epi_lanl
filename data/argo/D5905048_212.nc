CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-19T00:35:44Z creation;2018-02-19T00:35:49Z conversion to V3.1;2019-12-19T07:44:50Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180219003544  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_212                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Msɓ� 1   @�Mt����@3GKƧ��dLxF�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��AA?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C�)C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C`\Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�
D)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDdw
Dd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D��D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dػ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�;�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�HR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�%A�1A�%A�%A�A�%A�A�%A�%A�%A�1A�1A�1A�
=A�
=A�1A�
=A�
=A�JA�JA�
=A�
=A�
=A���A�A�G�AǾwA� �A�ƨA�K�A�1'A�/A���A��AŴ9A�E�A��A�"�A�"�A��A��A��A�%A��A���AľwAİ!A�VA��A×�A���A´9A�ZA�bNA��DA�$�A�7LA�ffA�VA�{A�p�A�
=A��A�bNA�?}A��TA�|�A�oA�A�A��#A���A�"�A�bNA�S�A���A��yA�M�A�XA���A�jA��A�9XA��A�  A���A��A���A�jA��A��\A�bNA��#A�bA�z�A�\)A�oA���A�(�A�%A�p�A�K�A���A�1A�A��uA�A���A�K�A�+A�  A~��A}A{l�Ax1'Ar��Ap��An1'Akp�AjffAi��Ag��Afn�Ae�AcAb��Aa�A_ƨA^ffA\$�AX�AV=qAUt�AT��AR(�AP��AM��AK�AH�yAE`BAC�-ABJA@��A?�^A>n�A=t�A<��A;�TA:�+A:�A9�mA9G�A8JA7G�A6�A5"�A4��A4ȴA4��A3A2�/A2�`A2~�A1�A/?}A+��A)?}A(z�A'�A&Q�A$��A"�A�wA  A�;A�
AA\)AI�A�hA�A��AXA��A��A��An�A��A�A��A\)A��A�\A�9A�`AJA
1A	XA�;A�/A1A�A�AZA�A�wAl�A&�A�HAdZA E�@��@���@�`B@��@�z�@���@�@��j@���@�"�@��H@���@�u@�5?@��@��@�^@��@� �@�@���@�^@�7L@�%@�r�@ޗ�@�1@�M�@�33@��@�hs@��@�(�@Ӿw@ӝ�@�\)@ҸR@�{@���@�1@�dZ@�5?@�?}@�5?@���@ȼj@�r�@ȣ�@���@ȃ@�Q�@�J@�7L@�Q�@\@�-@�@��@Ƈ+@ƸR@Ə\@�=q@�-@�x�@�`B@��@�I�@�  @�ƨ@���@���@��@�b@��@�;d@�
=@���@���@��@��P@��F@��@���@�~�@��!@�(�@�j@��D@���@��`@��9@�1'@�@���@�+@��^@�(�@��m@�|�@��!@���@��@��@���@��@��+@�5?@���@�@�x�@�&�@��j@�r�@�1@���@�t�@�S�@�C�@�S�@�l�@�S�@��@��@�M�@�@��@�?}@���@��@�%@��@�z�@��@�C�@��@���@�E�@�@��@�@���@���@�Ĝ@��/@�G�@���@�A�@�9X@��
@�o@���@��R@���@�~�@�M�@�=q@�-@��@�O�@���@���@��9@��@��j@���@��`@���@���@�I�@�ƨ@�K�@���@�ȴ@��!@���@��@���@�G�@��@��@���@��@��`@��j@��D@�r�@�I�@���@�|�@�|�@��@��@��y@��@�n�@�M�@��T@�X@��@���@��9@��u@�9X@�  @�ƨ@���@���@�S�@�+@���@�
=@�@��@���@��R@�^5@�E�@�-@��@��T@��-@���@�O�@�/@���@���@�I�@��F@���@�\)@��y@��!@�ȴ@�ȴ@���@�^5@�$�@��7@�V@���@��D@�Q�@��@��P@�\)@�\)@�K�@�@���@�ff@�v�@�V@�=q@�-@��@���@�hs@���@��@���@�ƨ@���@��w@��m@���@�o@���@���@��!@�ȴ@��!@��@���@�G�@���@���@��@���@��D@�9X@���@�t�@��@��@�ȴ@���@�M�@�@��T@���@��^@��-@���@��7@�x�@�p�@��/@��@�r�@�I�@� �@���@��F@�\)@�"�@���@��@��@�ȴ@���@���@���@�~�@�E�@���@��h@�G�@��u@�b@�@K�@~ff@}��@}�h@}`B@|�@|�/@|�D@|j@|Z@|j@|�@{�
@{ƨ@{ƨ@{�@{dZ@z��@y��@yG�@x�`@x�u@xQ�@w��@w;d@w+@w
=@v��@vff@t�@tZ@st�@so@r�\@q��@qhs@pĜ@p�u@pQ�@o|�@n��@mO�@m�@m�@l��@l1@k"�@j��@i�#@ihs@i%@hA�@gK�@fff@e�-@e?}@d��@d�j@d�@d��@dz�@d9X@d1@c��@c�m@c��@c��@c�@ct�@c33@co@b�@b�!@b-@a��@a��@aX@a%@`��@`��@` �@_|�@_K�@_
=@^�+@^ff@^V@^5?@]�-@]/@\�@\j@\Z@\I�@[��@[C�@[o@Z�!@Z^5@Z�@Y�#@Y�^@Y��@Yx�@Y7L@Y%@XĜ@XĜ@X�9@X��@Xr�@X1'@W��@W��@W+@V��@VE�@V@T��@TI�@S��@S"�@R�\@R^5@R=q@Q�#@Q�^@Qx�@QX@Q&�@PĜ@P�@O��@O+@O�@Nȴ@Nv�@NE�@N{@M�@M�T@M��@M��@M�h@L��@L(�@K�m@Kƨ@K�F@K��@K��@KC�@J^5@I�@I��@IG�@I�@H��@H�`@H��@HĜ@H�u@H  @G|�@F��@F�y@Fȴ@Fv�@F@E�@E�-@EV@D�j@D��@Dz�@D9X@D�@C��@C�
@C�@Ct�@CdZ@Co@B~�@A��@A��@A�^@Ax�@AX@A&�@@�9@@r�@@  @?|�@?�@>ȴ@>��@>5?@=�@=�T@=@=�h@=?}@<�@<�/@<�j@<��@<�D@<z�@<z�@<j@<j@<Z@<(�@;��@;�
@;�m@;�F@;t�@;33@;"�@:��@:^5@:-@9��@9��@9%@8��@8�`@8��@8�9@8��@8�u@8r�@8Q�@8b@7�w@7��@7
=@6��@5�@5�@5O�@5/@5V@4��@4�/@4�j@4z�@3��@3�F@3S�@2�H@2�@1�@1��@1��@1��@1�7@1X@1�@0��@0�u@01'@/�w@/|�@/l�@/\)@/;d@.�y@.��@.v�@.V@.{@-�T@-@-�h@-V@,Z@,I�@+��@+�m@+ƨ@+ƨ@+dZ@+@*��@)�#@)�7@)7L@)&�@)%@(��@(Ĝ@(��@(r�@(b@'�@'�P@'�P@'|�@'l�@'l�@';d@&��@&V@%�@%�h@%`B@%?}@%/@$��@$z�@$Z@$9X@#�m@#ƨ@#��@#�@#t�@#C�@#@"�!@"�\@"M�@"J@!��@!�7@!G�@!&�@ ��@ �u@ A�@ b@�w@l�@l�@l�@\)@;d@
=@�y@�R@��@V@�-@�@p�@p�@`B@`B@`B@`B@O�@?}@�@�@��@�@��@�@(�@ƨ@o@��@�\@~�@~�@~�@n�@=q@�@�7@7L@%@��@�`@��@�9@��@�u@�@�@1'@��@|�@l�@\)@K�@K�@��@�@ȴ@ȴ@�R@�R@��@�+@E�@@{@{@�@@�-@��@�h@�h@�@�@O�@V@��@�@��@�D@j@Z@Z@I�@�@��@ƨ@��@S�@@��@��@n�@M�@M�@-@�@J@�@J@��@�@�@��@��@G�@7L@7L@7L@�@Ĝ@�@A�@ �@b@b@  @�;@��@�w@��@l�@�@ȴ@�R@��@ff@E�@5?@$�@$�@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�%A�1A�%A�%A�A�%A�A�%A�%A�%A�1A�1A�1A�
=A�
=A�1A�
=A�
=A�JA�JA�
=A�
=A�
=A���A�A�G�AǾwA� �A�ƨA�K�A�1'A�/A���A��AŴ9A�E�A��A�"�A�"�A��A��A��A�%A��A���AľwAİ!A�VA��A×�A���A´9A�ZA�bNA��DA�$�A�7LA�ffA�VA�{A�p�A�
=A��A�bNA�?}A��TA�|�A�oA�A�A��#A���A�"�A�bNA�S�A���A��yA�M�A�XA���A�jA��A�9XA��A�  A���A��A���A�jA��A��\A�bNA��#A�bA�z�A�\)A�oA���A�(�A�%A�p�A�K�A���A�1A�A��uA�A���A�K�A�+A�  A~��A}A{l�Ax1'Ar��Ap��An1'Akp�AjffAi��Ag��Afn�Ae�AcAb��Aa�A_ƨA^ffA\$�AX�AV=qAUt�AT��AR(�AP��AM��AK�AH�yAE`BAC�-ABJA@��A?�^A>n�A=t�A<��A;�TA:�+A:�A9�mA9G�A8JA7G�A6�A5"�A4��A4ȴA4��A3A2�/A2�`A2~�A1�A/?}A+��A)?}A(z�A'�A&Q�A$��A"�A�wA  A�;A�
AA\)AI�A�hA�A��AXA��A��A��An�A��A�A��A\)A��A�\A�9A�`AJA
1A	XA�;A�/A1A�A�AZA�A�wAl�A&�A�HAdZA E�@��@���@�`B@��@�z�@���@�@��j@���@�"�@��H@���@�u@�5?@��@��@�^@��@� �@�@���@�^@�7L@�%@�r�@ޗ�@�1@�M�@�33@��@�hs@��@�(�@Ӿw@ӝ�@�\)@ҸR@�{@���@�1@�dZ@�5?@�?}@�5?@���@ȼj@�r�@ȣ�@���@ȃ@�Q�@�J@�7L@�Q�@\@�-@�@��@Ƈ+@ƸR@Ə\@�=q@�-@�x�@�`B@��@�I�@�  @�ƨ@���@���@��@�b@��@�;d@�
=@���@���@��@��P@��F@��@���@�~�@��!@�(�@�j@��D@���@��`@��9@�1'@�@���@�+@��^@�(�@��m@�|�@��!@���@��@��@���@��@��+@�5?@���@�@�x�@�&�@��j@�r�@�1@���@�t�@�S�@�C�@�S�@�l�@�S�@��@��@�M�@�@��@�?}@���@��@�%@��@�z�@��@�C�@��@���@�E�@�@��@�@���@���@�Ĝ@��/@�G�@���@�A�@�9X@��
@�o@���@��R@���@�~�@�M�@�=q@�-@��@�O�@���@���@��9@��@��j@���@��`@���@���@�I�@�ƨ@�K�@���@�ȴ@��!@���@��@���@�G�@��@��@���@��@��`@��j@��D@�r�@�I�@���@�|�@�|�@��@��@��y@��@�n�@�M�@��T@�X@��@���@��9@��u@�9X@�  @�ƨ@���@���@�S�@�+@���@�
=@�@��@���@��R@�^5@�E�@�-@��@��T@��-@���@�O�@�/@���@���@�I�@��F@���@�\)@��y@��!@�ȴ@�ȴ@���@�^5@�$�@��7@�V@���@��D@�Q�@��@��P@�\)@�\)@�K�@�@���@�ff@�v�@�V@�=q@�-@��@���@�hs@���@��@���@�ƨ@���@��w@��m@���@�o@���@���@��!@�ȴ@��!@��@���@�G�@���@���@��@���@��D@�9X@���@�t�@��@��@�ȴ@���@�M�@�@��T@���@��^@��-@���@��7@�x�@�p�@��/@��@�r�@�I�@� �@���@��F@�\)@�"�@���@��@��@�ȴ@���@���@���@�~�@�E�@���@��h@�G�@��u@�b@�@K�@~ff@}��@}�h@}`B@|�@|�/@|�D@|j@|Z@|j@|�@{�
@{ƨ@{ƨ@{�@{dZ@z��@y��@yG�@x�`@x�u@xQ�@w��@w;d@w+@w
=@v��@vff@t�@tZ@st�@so@r�\@q��@qhs@pĜ@p�u@pQ�@o|�@n��@mO�@m�@m�@l��@l1@k"�@j��@i�#@ihs@i%@hA�@gK�@fff@e�-@e?}@d��@d�j@d�@d��@dz�@d9X@d1@c��@c�m@c��@c��@c�@ct�@c33@co@b�@b�!@b-@a��@a��@aX@a%@`��@`��@` �@_|�@_K�@_
=@^�+@^ff@^V@^5?@]�-@]/@\�@\j@\Z@\I�@[��@[C�@[o@Z�!@Z^5@Z�@Y�#@Y�^@Y��@Yx�@Y7L@Y%@XĜ@XĜ@X�9@X��@Xr�@X1'@W��@W��@W+@V��@VE�@V@T��@TI�@S��@S"�@R�\@R^5@R=q@Q�#@Q�^@Qx�@QX@Q&�@PĜ@P�@O��@O+@O�@Nȴ@Nv�@NE�@N{@M�@M�T@M��@M��@M�h@L��@L(�@K�m@Kƨ@K�F@K��@K��@KC�@J^5@I�@I��@IG�@I�@H��@H�`@H��@HĜ@H�u@H  @G|�@F��@F�y@Fȴ@Fv�@F@E�@E�-@EV@D�j@D��@Dz�@D9X@D�@C��@C�
@C�@Ct�@CdZ@Co@B~�@A��@A��@A�^@Ax�@AX@A&�@@�9@@r�@@  @?|�@?�@>ȴ@>��@>5?@=�@=�T@=@=�h@=?}@<�@<�/@<�j@<��@<�D@<z�@<z�@<j@<j@<Z@<(�@;��@;�
@;�m@;�F@;t�@;33@;"�@:��@:^5@:-@9��@9��@9%@8��@8�`@8��@8�9@8��@8�u@8r�@8Q�@8b@7�w@7��@7
=@6��@5�@5�@5O�@5/@5V@4��@4�/@4�j@4z�@3��@3�F@3S�@2�H@2�@1�@1��@1��@1��@1�7@1X@1�@0��@0�u@01'@/�w@/|�@/l�@/\)@/;d@.�y@.��@.v�@.V@.{@-�T@-@-�h@-V@,Z@,I�@+��@+�m@+ƨ@+ƨ@+dZ@+@*��@)�#@)�7@)7L@)&�@)%@(��@(Ĝ@(��@(r�@(b@'�@'�P@'�P@'|�@'l�@'l�@';d@&��@&V@%�@%�h@%`B@%?}@%/@$��@$z�@$Z@$9X@#�m@#ƨ@#��@#�@#t�@#C�@#@"�!@"�\@"M�@"J@!��@!�7@!G�@!&�@ ��@ �u@ A�@ b@�w@l�@l�@l�@\)@;d@
=@�y@�R@��@V@�-@�@p�@p�@`B@`B@`B@`B@O�@?}@�@�@��@�@��@�@(�@ƨ@o@��@�\@~�@~�@~�@n�@=q@�@�7@7L@%@��@�`@��@�9@��@�u@�@�@1'@��@|�@l�@\)@K�@K�@��@�@ȴ@ȴ@�R@�R@��@�+@E�@@{@{@�@@�-@��@�h@�h@�@�@O�@V@��@�@��@�D@j@Z@Z@I�@�@��@ƨ@��@S�@@��@��@n�@M�@M�@-@�@J@�@J@��@�@�@��@��@G�@7L@7L@7L@�@Ĝ@�@A�@ �@b@b@  @�;@��@�w@��@l�@�@ȴ@�R@��@ff@E�@5?@$�@$�@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BI�BH�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BI�BH�BG�BH�Be`B�{B��B��B�!B�!B�9B�LB�9B�RB�qBȴB�)B�B�B��B��B��B��BBuB2-BD�BM�B`BBk�BjBp�BiyBR�B#�B�BL�BE�B33B\)BZBO�BO�BVBE�B5?B2-B49B%�B1BBPBbBBBB+BoB+BuB�B{B%B�5B�ZBɺB��B��B�=B`BBQ�BT�BK�BB�B!�B
�`B
�ZB
�-B
��B
��B
��B
��B
��B
�JB
~�B
�B
gmB
gmB
P�B
!�B
0!B
�B
B	�mB	�B	�RB	��B	��B	��B	��B	�VB	�B	{�B	o�B	iyB	^5B	G�B	>wB	)�B	uB	\B	)�B	+B	uB	B�B�)BƨB��B�B��B��B��B��B��B�^B�RB�dB��B�ZB�sB�yB��B��B��B	B	 �B	�B	oB	PB	�B	PB��B�ZB�9B�B�dB�XB��B��B�B�VB�PB��B��B��B��B�uB�bB�hB�DBy�BgmBr�BhsB`BBl�Bt�Bw�Bm�B_;B|�B�B� Bu�BcTBq�BcTB]/B]/BbNB^5BW
B_;B_;B]/BZBXBF�BI�BM�B\)B]/B[#BXBT�BO�BE�BA�BH�BZBS�BM�BD�B9XBH�BZB`BB\)BaHB`BB`BBdZBffB`BBXBT�BW
BXBdZBo�Bq�Br�Bw�Bz�By�Bu�Bv�Br�By�B}�B�B� B{�B�+B��B��B��B�B��B��B��B��B�B��B�dB��B�ZB�B�B�B�mB�ZB�NB��B��B�B�yB�fB�5B��B��B��B��B�B�5B�BB�TB�B�B�B�B�B��B��B	1B	
=B	DB	\B	hB	VB		7B	B	VB	�B		7B	B	
=B	DB	%B	B��B	  B	B	B	1B	JB	VB	\B	uB	�B	�B	�B	�B	"�B	'�B	)�B	+B	/B	0!B	0!B	33B	7LB	6FB	9XB	=qB	?}B	@�B	C�B	D�B	G�B	I�B	E�B	J�B	K�B	L�B	R�B	W
B	[#B	aHB	`BB	`BB	gmB	k�B	q�B	u�B	s�B	|�B	z�B	y�B	|�B	�B	�+B	�=B	�VB	�oB	�oB	�hB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�FB	�RB	�RB	�^B	��B	��B	��B	��B	��B	�}B	��B	ÖB	ÖB	��B	B	ŢB	ÖB	ŢB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�;B	�;B	�BB	�;B	�TB	�ZB	�ZB	�TB	�ZB	�fB	�ZB	�fB	�`B	�mB	�fB	�ZB	�yB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
B
B
%B
+B
%B
B
+B
%B
1B
DB
JB
VB
PB
DB

=B
DB
JB
PB
\B
VB
VB
\B
oB
oB
uB
uB
uB
uB
uB
oB
\B
hB
oB
oB
uB
uB
uB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
oB
bB
bB
{B
oB
hB
{B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
 �B
 �B
�B
!�B
 �B
"�B
#�B
!�B
$�B
$�B
%�B
$�B
"�B
"�B
"�B
&�B
(�B
(�B
&�B
%�B
(�B
&�B
(�B
(�B
'�B
&�B
(�B
(�B
+B
,B
,B
-B
-B
-B
,B
-B
-B
-B
-B
-B
.B
-B
-B
-B
-B
-B
,B
-B
-B
-B
.B
.B
.B
-B
-B
/B
0!B
/B
1'B
1'B
0!B
/B
/B
0!B
2-B
33B
2-B
0!B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
33B
6FB
6FB
8RB
8RB
;dB
;dB
:^B
;dB
;dB
<jB
;dB
;dB
;dB
:^B
;dB
>wB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
<jB
=qB
@�B
A�B
A�B
A�B
@�B
?}B
=qB
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
E�B
D�B
D�B
C�B
E�B
D�B
C�B
E�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
F�B
E�B
G�B
I�B
I�B
H�B
I�B
I�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
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
N�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
N�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
O�B
O�B
P�B
Q�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
R�B
S�B
S�B
S�B
R�B
VB
W
B
XB
XB
W
B
W
B
VB
W
B
W
B
W
B
W
B
XB
YB
YB
YB
XB
XB
YB
YB
YB
YB
ZB
YB
XB
XB
[#B
[#B
\)B
\)B
\)B
ZB
ZB
[#B
YB
\)B
]/B
_;B
_;B
_;B
^5B
_;B
_;B
^5B
_;B
`BB
aHB
aHB
aHB
`BB
_;B
^5B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
aHB
bNB
cTB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
e`B
e`B
e`B
dZB
e`B
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
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
hsB
hsB
ffB
gmB
gmB
iyB
k�B
k�B
k�B
k�B
k�B
jB
jB
iyB
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
k�B
jB
m�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
q�B
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
s�B
r�B
r�B
s�B
t�B
t�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BI�BH�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BI�BH�BG�BIRBf2B��B��B�8B��B��B�nB�fB��B��B��BɆB�]B�cB�B��B��B��B�	BMB�B2GBD�BN�Ba-BlWBk�Bq[Bj�BU2B'RB	BMBG�B6B\CB[�BR�BRTBWsBHfB8�B5�B7fB)yBDB%B(B BYB�B�BfB&B,�ByBkB�B	B�B��B��B�-B�wB�BfBT�BVmBM�BEB&�B
�B
�B
�RB
��B
�@B
�pB
��B
��B
�\B
��B
��B
jKB
h�B
S�B
&�B
1�B
"4B
�B	�B	�9B	�0B	�B	��B	�TB	��B	�bB	�B	}�B	q[B	j�B	_�B	J=B	@�B	-CB	YB	oB	*�B	,�B	�B	�B�`B�;B�=B�)B�/B�B��B�B��B�sB�dB��B�B�}B��B�B�B��B�B�dB	�B	 �B	#B	�B	pB	�B	VB��B�B��B��B�jB�^B�@B�BB��B� B�\B�$B�$B�,B�~B��B�hB�:B�JB|BjeBt9Bj�Bb�Bm�Bu�Bx�Bo�BbB}VB�B�iBw2Be�Br�Be`B^�B^5Bb�B_VBX+B_�B_�B]�BZ�BX�BH�BK^BO\B\�B]~B[�BX�BU�BP�BGzBC�BJ=BZ�BT�BOBF�B<6BJ�BZ�B`�B\�Ba�B`�B`�Bd�Bf�BaBY�BV�BXyBZBe,BpBrBsMBxB{Bz*Bv`BwfBs�BzxB~�B��B�B}�B��B��B��B��B�B�XB�yB�\B��B��B�B�B�.B�:B��B�B��B��B��B�TB�gB�6B�B��B��B�;BѝB̳B̳B�aB�QB�jB�vB�TB�B�UB��B�/B��B��B��B	�B	
#B	DB	BB	�B	�B		�B	[B	"B	�B	
XB	B	
�B	�B	�B	�B��B	 �B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#:B	(
B	*B	+6B	/5B	0!B	0UB	3�B	7�B	6�B	9�B	=�B	?�B	@�B	C�B	D�B	G�B	J=B	FYB	J�B	LB	MB	S&B	WYB	[=B	abB	`�B	`�B	g�B	k�B	q�B	vB	t9B	}B	{JB	z^B	}<B	�B	�EB	�rB	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�2B	�$B	�=B	�"B	�CB	�wB	��B	�hB	�`B	��B	��B	�^B	��B	��B	��B	��B	��B	� B	��B	ÖB	ðB	��B	��B	żB	��B	��B	�B	�B	��B	�"B	��B	�B	�BB	�4B	�B	�B	�B	�MB	�+B	�7B	�IB	�jB	�VB	�VB	�\B	ߊB	�nB	�tB	�tB	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�*B	�B	��B	�B	�B	�B	�(B	�(B	�HB	�jB	�]B	�qB	�B
'B
B
B
uB
 iB
UB
9B
%B
+B
tB
�B
_B
�B
�B
^B
~B
VB
jB
�B

�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
 �B
�B
�B
B
B
�B
B
�B
�B
B
�B
!�B
"�B
 �B
 �B
IB
!�B
!-B
# B
$B
"B
$�B
%B
%�B
%B
#:B
#:B
#:B
'B
)B
)*B
'8B
&2B
)DB
'8B
)*B
)DB
(>B
'RB
)DB
)DB
+6B
,"B
,"B
-B
-CB
-)B
,=B
-)B
-B
-)B
-CB
-)B
.B
-)B
-CB
-)B
-CB
-)B
,=B
-CB
-CB
-)B
.IB
./B
.IB
-]B
-]B
/OB
0;B
/OB
1[B
1AB
0UB
/OB
/OB
0UB
2GB
3MB
2GB
0oB
2GB
3MB
3hB
3MB
4TB
4TB
5ZB
5tB
5tB
5ZB
5tB
5tB
6FB
6FB
6FB
5ZB
5ZB
5tB
5ZB
5tB
5tB
5tB
5tB
3�B
6zB
6zB
8�B
8�B
;�B
;�B
:xB
;B
;B
<�B
;B
;�B
;B
:�B
;�B
>�B
=�B
=�B
>�B
?�B
?�B
?}B
?}B
?�B
>�B
<�B
=�B
@�B
A�B
A�B
A�B
@�B
?�B
=�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
E�B
D�B
D�B
C�B
E�B
D�B
C�B
E�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
F�B
E�B
G�B
I�B
I�B
H�B
I�B
I�B
H�B
H�B
H�B
J	B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
NB
OB
N�B
N�B
N�B
N�B
N�B
OB
N�B
N�B
PB
O�B
N�B
N�B
N�B
O�B
OB
OB
PB
PB
P.B
OB
Q�B
Q�B
Q�B
RB
RB
RB
R B
RB
Q B
Q B
QB
PB
PB
QB
R:B
TB
TB
T�B
T�B
UB
TB
T,B
S&B
TB
T,B
T,B
S[B
V9B
W$B
X+B
X+B
W?B
W$B
VB
W$B
W?B
W?B
WYB
X+B
YB
Y1B
Y1B
X+B
X+B
Y1B
YKB
Y1B
YKB
Z7B
Y1B
XEB
XEB
[#B
[WB
\)B
\CB
\CB
ZQB
ZQB
[=B
YeB
\CB
]IB
_;B
_VB
_;B
^jB
_pB
_VB
^OB
_VB
`\B
aHB
aHB
abB
`\B
_VB
^�B
`\B
`vB
abB
a|B
bhB
b�B
bhB
a|B
bhB
cnB
bhB
c�B
cnB
cnB
dZB
cnB
cnB
cnB
dtB
d�B
c�B
dtB
dtB
ezB
ezB
ezB
d�B
e�B
f�B
ezB
f�B
gmB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
f�B
f�B
h�B
i�B
iyB
iyB
i�B
iyB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
h�B
h�B
f�B
g�B
g�B
i�B
k�B
k�B
k�B
k�B
k�B
j�B
j�B
i�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
k�B
j�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
q�B
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
s�B
r�B
r�B
s�B
t�B
t�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802230036432018022300364320180223003643201806221326322018062213263220180622132632201804050730142018040507301420180405073014  JA  ARFMdecpA19c                                                                20180219093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180219003544  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180219003546  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180219003547  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180219003548  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180219003548  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180219003548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180219003548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180219003549  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180219003549                      G�O�G�O�G�O�                JA  ARUP                                                                        20180219005619                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180219153555  CV  JULD            G�O�G�O�F�k�                JM  ARCAJMQC2.0                                                                 20180222153643  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180222153643  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223014  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042632  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                