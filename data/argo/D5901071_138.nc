CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:29Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142313  20190522121827  1727_5046_138                   2C  D   APEX                            2143                            040306                          846 @��:��1   @����?�@7�     �c�x���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @���A   A   A@  Aa��A���A�  A�  A���A���A���A���A���B ffB  B  B��B   B(  B0  B8ffB@  BH  BPffBX  B`  Bh  BpffBxffB�  B�  B���B�  B�  B�  B���B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�33B�  B���B�  B�  B�  C   C  C�fC  C�C
  C  C  C  C  C�C�C  C�fC  C�C   C!�fC$  C&  C(�C*�C,�C.�C0  C1�fC4  C6  C8�C:  C;�fC>  C@�CB  CD  CF�CH  CJ  CL  CN  CP  CR  CS�fCU�fCW�fCZ  C\�C^  C_�fCb  Cd�Cf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C��C��C��3C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C�  C��3C��3C��C��C�  C��C��C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C��3C�  C�  C��C��C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��3D � D  Dy�D  D� D  D�fDfD�fDfD�fD  Dy�D  D� D  D� D	fD	� D	��D
� DfD� D  D�fD  D� DfD�fD  D� D  D� D  D� D  D�fDfD�fD  D� D  Dy�D��D� DfD�fD  Dy�D��Dy�D  D� D  D�fD  Dy�D  D� D  D�fD  Dy�D��D � D!  D!� D!��D"� D#  D#� D$  D$y�D%  D%� D%��D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1fD1�fD2  D2� D3  D3� D4  D4y�D5  D5� D5��D6� D7  D7y�D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DEy�DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D�0 D�\�D���D��3D�0 D�ffD���D��fD�0 D�` D���D���D�&fD�Y�Dڠ D�ٚD�#3D�I�D� D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@�33@�  A��A!��AA��Ac33A���A���A���A���A���Aљ�AᙚA�B ��BffBffB  B ffB(ffB0ffB8��B@ffBHffBP��BXffB`ffBhffBp��Bx��B�33B�33B�  B�33B�33B�33B�  B�  B�33B�ffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�ffB�ffB�33B�  B�33B�33B�33C �C�C  C�C33C
�C�C�C�C�C33C33C�C  C�C33C �C"  C$�C&�C(33C*33C,33C.33C0�C2  C4�C6�C833C:�C<  C>�C@33CB�CD�CF33CH�CJ�CL�CN�CP�CR�CT  CV  CX  CZ�C\33C^�C`  Cb�Cd33Cf�Ch�Cj�Cl�Cn  Cp  Cr�Ct�Cv�Cx�Cz�C|�C~  C��C��C�  C��C��C�  C��C��C��C��C��C�  C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C�  C��C��C�  C�  C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C�  C��C��C��C��C�  C��C��C��C��C�  C�  C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C�  C��C�  C�  C��C��C��C��C��C��C�  C��C��C�  C��C��D   D �fDfD� DfD�fDfD��D�D��D�D��DfD� DfD�fDfD�fD	�D	�fD
  D
�fD�D�fDfD��DfD�fD�D��DfD�fDfD�fDfD�fDfD��D�D��DfD�fDfD� D  D�fD�D��DfD� D  D� DfD�fDfD��DfD� DfD�fDfD��DfD� D   D �fD!fD!�fD"  D"�fD#fD#�fD$fD$� D%fD%�fD&  D&�fD'fD'�fD(�D(�fD)fD)�fD*fD*�fD+fD+��D,fD,�fD-fD-�fD.�D.�fD/fD/�fD0fD0�fD1�D1��D2fD2�fD3fD3�fD4fD4� D5fD5�fD6  D6�fD7fD7� D8fD8�fD9fD9�fD:�D:�fD;fD;�fD<fD<�fD=fD=� D>fD>�fD?fD?�fD@�D@�fDAfDA�fDBfDB�fDCfDC�fDDfDD��DEfDE� DFfDF��DGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM� DNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR��DSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]  D]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDf�Df�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDk�Dk�fDlfDl�fDmfDm�fDnfDn� DofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDy��D�33D�` D�� D��fD�33D�i�D�� D���D�33D�c3D���D�� D�)�D�\�Dڣ3D���D�&fD�L�D�3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�  A���A�  A�  A���A�  A�%A�1A�  A���A���A��yA���A��+A�\)A�E�A�JA���A��A��mA��/A���A�A��RA��RA��-A���A���A��DA��A��A�|�A�t�A�p�A�p�A�n�A�hsA�dZA�bNA�`BA�^5A�C�A��;A��A�&�A�
=A�jA���A��A�n�A��A��/A��yA��+A�C�A�7LA���A��A��A�+A�{A�  A���A�S�A��HA�t�A�jA���A���A�{A���A�O�A��RA�`BA�/A��A���A���A��FA�|�A�z�A�^5A��\A��7A���A�;dA���A�
=A�&�A�v�A���A��A�33A��;A��A�dZA���A��DA�ƨA���A���A�VA�I�A�M�A��TA��/A��A��-A��`A��yA�33A�oA�S�A�
=A��A�E�A���A�VA��A���A��FA�A���A�;dA���A�^5A�bNA��A~�\A|  Az��Ay��Ax��Aw�-AtbNAq\)Ap��Ao�Am�7AmC�AmAlbNAj1'Agt�Ae;dAc�Ab�RA`�jA_��A^��A]�A\ �AY7LAXn�AW7LAV�AU&�ASC�APQ�AN�\AM��AL~�AK33AI�mAF�yAEK�ADVAC&�AB  AA��A@��A@$�A?��A?&�A=l�A<-A:��A8�uA7�7A6�A6�A4��A3��A2�\A2�A1ƨA1O�A0��A0E�A/��A/O�A.�A-�#A-"�A,��A+�mA+�7A*�DA(�uA(-A't�A%��A%
=A$�9A"��A!��A ~�A�hAȴAO�A�/A�9A�TA�RA�TAdZA/A�RA�^A��A��A{A33A��A5?AO�A1'A|�A�HA�uAbNAAdZA33A
�/A	dZA|�AC�A�AQ�A��AA�RA��A�\AjA��A33@�S�@��@�t�@�\)@�ȴ@�J@��@��F@���@��@��@�@�\@���@�@蛦@�9X@�1'@��@�+@�@� �@��@�7L@��m@�E�@�A�@��@ڇ+@��@�p�@��@�|�@�ff@Դ9@�"�@҇+@�V@�J@ѡ�@��@�r�@ϕ�@�$�@�hs@���@�A�@�C�@�S�@���@ģ�@�bN@�bN@�1'@ÍP@���@�=q@��7@��@��;@�M�@��T@��-@�%@���@���@��@��@��j@�S�@�ff@�J@���@���@��/@��@�j@�9X@�"�@�v�@��@�I�@���@�S�@��!@�^5@��#@���@�9X@�dZ@���@�@�%@���@�A�@�l�@�C�@�@���@�ff@�-@�/@���@�dZ@�C�@���@�`B@�Ĝ@��@��P@��@�n�@�{@���@��7@�O�@�%@���@���@�j@��;@�l�@��y@�v�@�@�@�X@�&�@��/@�Ĝ@��u@�9X@���@��;@���@�|�@�+@���@�M�@��#@�O�@��@��/@���@��D@�1@���@�C�@�+@�"�@�o@���@��H@���@��@�x�@�O�@�X@�/@�%@��9@�bN@�r�@�Z@�Q�@�1@��@��@�"�@���@��!@��\@�~�@�n�@�$�@���@���@�X@��@�G�@��@���@�hs@�O�@���@��u@��@���@���@���@�S�@��@�ȴ@�ȴ@�ȴ@���@��!@��\@�n�@�$�@���@���@���@�`B@�/@��`@�Ĝ@��j@��u@�1'@�  @��m@�ƨ@��P@�\)@��@���@��!@���@��+@�~�@�~�@�n�@�V@�{@���@�/@��@��u@��D@��u@���@���@���@���@��@��9@��D@�A�@��;@�@zn�@tZ@j-@b=q@[�m@W��@Nff@GK�@A%@:�@3t�@+�m@&��@"-@$�@�@$�@x�@�h@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�  A���A�  A�  A���A�  A�%A�1A�  A���A���A��yA���A��+A�\)A�E�A�JA���A��A��mA��/A���A�A��RA��RA��-A���A���A��DA��A��A�|�A�t�A�p�A�p�A�n�A�hsA�dZA�bNA�`BA�^5A�C�A��;A��A�&�A�
=A�jA���A��A�n�A��A��/A��yA��+A�C�A�7LA���A��A��A�+A�{A�  A���A�S�A��HA�t�A�jA���A���A�{A���A�O�A��RA�`BA�/A��A���A���A��FA�|�A�z�A�^5A��\A��7A���A�;dA���A�
=A�&�A�v�A���A��A�33A��;A��A�dZA���A��DA�ƨA���A���A�VA�I�A�M�A��TA��/A��A��-A��`A��yA�33A�oA�S�A�
=A��A�E�A���A�VA��A���A��FA�A���A�;dA���A�^5A�bNA��A~�\A|  Az��Ay��Ax��Aw�-AtbNAq\)Ap��Ao�Am�7AmC�AmAlbNAj1'Agt�Ae;dAc�Ab�RA`�jA_��A^��A]�A\ �AY7LAXn�AW7LAV�AU&�ASC�APQ�AN�\AM��AL~�AK33AI�mAF�yAEK�ADVAC&�AB  AA��A@��A@$�A?��A?&�A=l�A<-A:��A8�uA7�7A6�A6�A4��A3��A2�\A2�A1ƨA1O�A0��A0E�A/��A/O�A.�A-�#A-"�A,��A+�mA+�7A*�DA(�uA(-A't�A%��A%
=A$�9A"��A!��A ~�A�hAȴAO�A�/A�9A�TA�RA�TAdZA/A�RA�^A��A��A{A33A��A5?AO�A1'A|�A�HA�uAbNAAdZA33A
�/A	dZA|�AC�A�AQ�A��AA�RA��A�\AjA��A33@�S�@��@�t�@�\)@�ȴ@�J@��@��F@���@��@��@�@�\@���@�@蛦@�9X@�1'@��@�+@�@� �@��@�7L@��m@�E�@�A�@��@ڇ+@��@�p�@��@�|�@�ff@Դ9@�"�@҇+@�V@�J@ѡ�@��@�r�@ϕ�@�$�@�hs@���@�A�@�C�@�S�@���@ģ�@�bN@�bN@�1'@ÍP@���@�=q@��7@��@��;@�M�@��T@��-@�%@���@���@��@��@��j@�S�@�ff@�J@���@���@��/@��@�j@�9X@�"�@�v�@��@�I�@���@�S�@��!@�^5@��#@���@�9X@�dZ@���@�@�%@���@�A�@�l�@�C�@�@���@�ff@�-@�/@���@�dZ@�C�@���@�`B@�Ĝ@��@��P@��@�n�@�{@���@��7@�O�@�%@���@���@�j@��;@�l�@��y@�v�@�@�@�X@�&�@��/@�Ĝ@��u@�9X@���@��;@���@�|�@�+@���@�M�@��#@�O�@��@��/@���@��D@�1@���@�C�@�+@�"�@�o@���@��H@���@��@�x�@�O�@�X@�/@�%@��9@�bN@�r�@�Z@�Q�@�1@��@��@�"�@���@��!@��\@�~�@�n�@�$�@���@���@�X@��@�G�@��@���@�hs@�O�@���@��u@��@���@���@���@�S�@��@�ȴ@�ȴ@�ȴ@���@��!@��\@�n�@�$�@���@���@���@�`B@�/@��`@�Ĝ@��j@��u@�1'@�  @��m@�ƨ@��P@�\)@��@���@��!@���@��+@�~�@�~�@�n�@�V@�{@���@�/@��@��u@��D@��u@���@���@���@���@��@��9@��D@�A�@��;@�@zn�@tZ@j-@b=q@[�m@W��@Nff@GK�@A%@:�@3t�@+�m@&��@"-@$�@�@$�@x�@�h@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��BBB��B��BB��B��B��BBBŢB��B��B��B��B�
B�B�B�
B�B�B�B�#B�#B�)B�/B�5B�;B�;B�;B�;B�BB�BB�HB�HB�HB�HB�BB�BB�;B�/B�B�NB�B�B�mB�ZB�TB�TB�BB�B��B��B��B�
B�#B�B��B�B�yB�yB�mB�5B�5B�;B�B�B��B��BŢB��B�BǮB�}B�LB�B��B��B�B��B�Bv�Bl�BbNB[#BS�BG�B<jB5?B)�B �B�B1B��B�B�sB�B��B�qB�B��B�JB�By�Bo�BcTBP�BA�B33B%�B{B
��B
�B
�TB
�#B
��B
��B
�B
�bB
y�B
r�B
n�B
jB
gmB
`BB
VB
L�B
G�B
C�B
;dB
7LB
1'B
)�B
�B
PB
+B
B	��B	��B	�B	�B	�BB	��B	ȴB	�wB	�XB	�B	��B	�hB	�B	r�B	iyB	l�B	dZB	YB	N�B	>wB	0!B	 �B	�B	�B	uB	hB	B��B��B�B�B�yB�mB�fB�TB�;B�)B�
B��B��BɺBƨBÖBB��BBBÖBB��B��B�wB�qB�jB�^B�LB�3B�B��B��B��B��B�uB�uB��B��B��B��B��B��B��B�oB�VB�PB�DB�1B�7B�=B�1B�B�B|�By�Bu�Bo�Bl�BjBhsBgmBffBe`BdZBbNBbNB`BB^5B[#BZBZBXBW
BVBR�BT�BT�BS�BR�BP�BO�BN�BN�BN�BN�BL�BK�BJ�BI�BG�BE�BB�BD�BB�BG�BG�BG�BF�BF�BE�BD�BA�BF�BD�BD�BF�BG�BI�BK�BM�BM�BM�BM�BP�BP�BR�BVBYBYBYBYBXBW
BW
BW
BZB[#B[#BZBYBaHBcTBffBiyBiyBiyBk�Bl�Bl�Bm�Bn�Br�Bv�Bw�Bv�Bv�By�By�By�Bz�B}�B� B�B�B�B�B�B�B�B�B�%B�=B�bB��B��B��B��B��B��B��B��B�B�B�'B�3B�3B�3B�LB�RB�XB�^BĜBƨBǮBȴBȴBǮBǮBǮBɺB��B��B�B�)B�;B�ZB�fB�sB�B�B�B�B��B��B��B	  B	B	%B	1B		7B	DB	JB	VB	bB	uB	{B	�B	�B	�B	�B	�B	"�B	'�B	)�B	+B	-B	-B	1'B	49B	8RB	<jB	>wB	?}B	?}B	?}B	?}B	C�B	G�B	H�B	K�B	M�B	N�B	Q�B	VB	W
B	ZB	[#B	_;B	bNB	cTB	ffB	gmB	jB	m�B	m�B	m�B	n�B	o�B	q�B	s�B	t�B	w�B	z�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�DB	�JB	�JB	�JB	�JB	�JB	�PB	�PB	�\B	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�^B	�qB	��B	B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	�)B	�B	��B
DB
{B
�B
!�B
$�B
2-B
;dB
A�B
I�B
P�B
T�B
ZB
_;B
cTB
gmB
m�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��BBB��B��BB��B��B��BBÖBȴB��B��B��B��B�B�B�B�B�B�B�B�#B�#B�/B�5B�5B�;B�;B�;B�;B�BB�BB�HB�HB�HB�HB�BB�HB�NB�TB�HB�B��B�B�B�sB�fB�mB�`B�NB�B��B��B�B�)B�BB�B�B�B�B�ZB�`B�B�5B�B�)B�#B��B�B�`B��BÖB�jB�FB��B�B�dB��B�VB�Bv�BiyBffBffBQ�BE�B?}B2-B.B$�BhB+B��B��B�`B�)B��B�qB��B��B�JB�7B� Bp�B\)BO�B@�B7LB(�BJB
�B
�B
�ZB
�BB
�
B
��B
��B
�B
x�B
t�B
p�B
q�B
l�B
^5B
W
B
W
B
J�B
D�B
@�B
;dB
?}B
-B
uB
hB
JB	��B	��B	��B	��B	�B	�;B	��B	ŢB	ŢB	�-B	��B	��B	�PB	�B	p�B	t�B	m�B	cTB	]/B	O�B	;dB	'�B	"�B	 �B	�B	"�B	VB	B��B��B�B�B�B�B�B�B�fB�NB�HB��B��B��B��BɺBȴBŢBŢBǮBƨBŢBÖBÖBB��B�}B�dB�RB�'B�'B�B��B��B��B��B��B�B��B��B��B��B��B��B�hB�{B�oB�PB�PB�PB�PB�PB�PB�B�B}�Bx�Bw�Bq�Bo�Bl�BjBgmBffBffBe`BcTBcTBe`BdZB\)B\)B\)B]/B_;B[#BVBT�BT�BVBVBZBVBR�BO�BO�BN�BO�BN�BM�BO�BB�BN�BL�BL�BG�BG�BF�BF�BE�BH�BA�BI�BI�BJ�BK�BG�BP�BK�BO�BM�BP�BQ�BP�BVBYBVBYBZB[#B[#BZBZB[#B\)BZB[#B[#B`BBe`BffBcTBffBiyBiyBiyBn�Bl�Bl�Bm�Bs�Bw�Bx�Bw�By�Bv�B{�B|�B}�Bz�B�B� B�B�B�B�B�B�B�%B�%B�7B�=B�uB��B��B��B��B��B��B��B�B�B�'B�3B�9B�3B�?B�RB�XB�^B�^BƨB��B��BɺBɺBǮBǮBɺB��B��B��B�B�)B�BB�ZB�fB�yB�B�B�B�B��B��B��B	B	%B	+B		7B	
=B	DB	JB	\B	hB	uB	�B	�B	�B	�B	�B	 �B	#�B	(�B	+B	,B	-B	/B	1'B	5?B	8RB	<jB	>wB	?}B	@�B	@�B	A�B	D�B	G�B	H�B	L�B	N�B	O�B	R�B	VB	W
B	ZB	\)B	`BB	cTB	dZB	gmB	hsB	jB	m�B	m�B	n�B	o�B	p�B	r�B	s�B	s�B	v�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�JB	�JB	�JB	�JB	�JB	�PB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�^B	�qB	��B	B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	�)B	�B	��B
DB
{B
�B
!�B
$�B
2-B
;dB
A�B
I�B
P�B
T�B
ZB
_;B
cTB
gmB
m�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<D��<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<D��<#�
<#�
<49X<�t�<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<e`B<49X<u<e`B<�o<T��<D��<#�
<u<�o<T��<49X<e`B<T��<�C�<��
<e`B<#�
<#�
<#�
<e`B<�1<�h<�1<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<u<#�
<#�
<#�
<#�
<�1<�o<#�
<#�
<#�
<#�
<#�
<#�
<�C�<�t�<T��<49X<#�
<D��<#�
<#�
<#�
<49X<�C�<#�
<#�
<#�
<#�
<e`B<�C�<49X<#�
<#�
<#�
<D��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447222012010314472220120103144722  AO  ARGQ                                                                        20111130142313  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142313  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144722  IP                  G�O�G�O�G�O�                