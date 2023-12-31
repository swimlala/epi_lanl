CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:49Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               #A   AO  20111130143838  20190522121828  1728_5048_035                   2C  D   APEX                            2142                            040306                          846 @ԁҸ��1   @ԁ�Q�_�@5�fffff�c@�hr�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@y��@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�33B�  B�  B���C   C�C�C�C  C
  C�C�C  C  C  C�C  C�fC  C  C�fC"�C$  C%�fC(  C*�C,  C.  C0  C2  C4  C6  C8  C:�C<�C=�fC?�fCB�CD�CE�fCG�fCJ  CL  CN  CP  CQ�fCT  CV  CW�fCZ  C\  C^  C_�fCa�fCc�fCf  Ch�Cj�Cl�Cn�Cp�Cr  Ct  Cu�fCx  Cz�C|  C}�fC�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C��3C�  C�  C��3C��3C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D  D� D	fD	� D
  D
�fD  D� D  D� D  D� D  D� D  Dy�D��D�fD  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  Dy�D  D� D��D� D  Dy�D  D� D  D�fD  Dy�D  D� D   D � D!  D!y�D"  D"� D"��D#y�D$  D$� D%  D%�fD&fD&� D&��D'� D(fD(� D(��D)� D*fD*� D+  D+� D,  D,y�D-  D-� D.  D.�fD/  D/y�D/��D0y�D0��D1� D2fD2� D3  D3� D4  D4� D4��D5y�D5��D6y�D7  D7� D8  D8�fD9  D9y�D9��D:y�D;  D;� D<fD<� D<��D=� D>fD>� D?fD?�fD@  D@� DA  DAy�DB  DB� DC  DCy�DD  DD�fDE  DE� DF  DF� DF��DGy�DG��DHy�DH��DIy�DI��DJ� DK  DK� DK��DL� DMfDM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DW��DXy�DY  DY�fDZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Dfy�Df��Dgy�Dh  Dh�fDifDi�fDjfDj�fDk  Dky�Dk��Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dw� Dy��D�fD�&fD�y�D��3D�� D�&fD�� D���D��D�)�D�\�DǶfD��fD�,�Dڌ�D�3D��3D� D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@Y��@�  @�  A��A8  AX  Ax  A�  A�  A�  A�  A�  A�33A�33A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  BfffBn  Bu��B~  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�33B�  B�  B���B�  C��C��C��C� C	� C��C��C� C� C� C��C� CffC� C� CffC!��C#� C%ffC'� C)��C+� C-� C/� C1� C3� C5� C7� C9��C;��C=ffC?ffCA��CC��CEffCGffCI� CK� CM� CO� CQffCS� CU� CWffCY� C[� C]� C_ffCaffCcffCe� Cg��Ci��Ck��Cm��Co��Cq� Cs� CuffCw� Cy��C{� C}ffC� C�� C�� C���C�� C��3C�� C�� C�� C���C�� C��3C�� C�� C�� C�� C�� C�� C��3C�� C���C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C��3C�� C���C�� C�� C�� C��3C�� C�� C�� C�� C��3C�� C�� C�� C�� C���C�� C�� C�� C���C�� C�� C�� C���C�� C�� C���C�� C�� C���C�� C�� C���C�� C�� C�� C�� C���C���C���C˳3C�� C�� Cγ3Cϳ3Cг3C�� C���C�� C�� Cճ3C�� C���C�� Cٳ3C�� C���C�� C�� C޳3C�� C�� C�3C�� C�� C�� C���C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� D ` D � D` D� D` D� D` D� D` DٚD` D� DY�D� D` D� D` D�fD	` D	� D
ffD
� D` D� D` D� D` D� D` D� DY�DٚDffD� D` D� D` D� D` D� D` D� DffD� DY�D� D` D� DY�D� D` DٚD` D� DY�D� D` D� DffD� DY�D� D` D� D ` D � D!Y�D!� D"` D"ٚD#Y�D#� D$` D$� D%ffD%�fD&` D&ٚD'` D'�fD(` D(ٚD)` D)�fD*` D*� D+` D+� D,Y�D,� D-` D-� D.ffD.� D/Y�D/ٚD0Y�D0ٚD1` D1�fD2` D2� D3` D3� D4` D4ٚD5Y�D5ٚD6Y�D6� D7` D7� D8ffD8� D9Y�D9ٚD:Y�D:� D;` D;�fD<` D<ٚD=` D=�fD>` D>�fD?ffD?� D@` D@� DAY�DA� DB` DB� DCY�DC� DDffDD� DE` DE� DF` DFٚDGY�DGٚDHY�DHٚDIY�DIٚDJ` DJ� DK` DKٚDL` DL�fDM` DM� DN` DN� DO` DO� DPY�DP� DQ` DQ� DRffDR� DS` DS� DT` DT� DU` DU� DV` DV� DWY�DWٚDXY�DX� DYffDY� DZ` DZ� D[Y�D[� D\` D\� D]` D]� D^Y�D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` DcٚDd` Dd� De` De� DfY�DfٚDgY�Dg� DhffDh�fDiffDi�fDjffDj� DkY�DkٚDl` Dl� Dm` Dm�fDn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du�fDvffDv� Dw` Dy��D��fD�fD�i�D��3D�� D�fD�p D���D�ٚD��D�L�DǦfD��fD��D�|�D�3D��3D�  D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aǟ�Aǥ�Aǩ�Aǧ�AǬAǮAǥ�AǕ�AǗ�AǙ�Aǥ�Aǧ�Aǩ�Aǰ!AǴ9AǶFAǰ!AǮAǝ�A�p�A�`BA�`BA�bNA�`BA�^5A�ZA�VA�S�A�O�A�E�A�33A��Aŗ�A��TA�+A�^5A���A���A�I�A���A�ĜA�z�A�+A��mA��A��jA��uA���A�E�A�  A��RA��A�A�A��A��mA��A�r�A���A�E�A��mA�v�A��A��9A�JA��yA��A��A�ffA���A��!A��A��A�&�A��TA��FA�dZA��A��uA��A���A��PA�n�A�VA�K�A��A���A���A��A���A��`A���A�
=A���A��A�l�A�A�hsA��!A��A�5?A�JA��uA���A���A�%A��A��7A�I�A�VA��PA���A��A���A��A��hA�XA���A���A�G�A���A�l�A���A��A�r�A�Q�A�v�A�~�A��A�%A�A�K�A{G�Ay�;Ax��Av�+Aq�AnbAlE�Ak�AjVAi�FAhz�AgO�Af9XAc�hAa�A`�A^��A]��A\v�AZ�AX��AWO�AU�wAS�-AQ�AP �AN�/AM�ALffAK�AIG�AF��AE"�AB{A?�A<�+A;&�A:{A9K�A8bNA7oA4�A2��A1��A1%A0 �A.�9A-�7A,��A,A*�DA)�A(�RA&�yA%��A$ffA#ƨA"�yA"$�A!p�A �A?}A=qAx�A�RA{Ar�A��A�A(�A��A�^A�AA�AC�A^5A�A7LA$�A��AA�A�uA��Ap�A?}A
��A
�A	"�A�^AI�A�9AhsAȴA�A �u@�M�@�G�@��@�l�@�&�@��m@�
=@�{@��7@���@�1@���@���@���@�@�ƨ@��@畁@�5?@�h@�V@�33@�@�Z@߅@��y@�5?@݉7@�&�@ܛ�@۝�@��@��@ա�@���@���@��@θR@�ȴ@�|�@� �@�r�@Л�@мj@�&�@�Q�@͑h@�ff@ɺ^@�X@��`@�j@ǶF@�\)@��@Ɨ�@�V@��@�/@��@Õ�@��y@�ff@��T@�`B@���@���@��@�v�@�$�@��@��T@���@�@�@���@��@�  @�|�@�"�@���@��@���@��@�O�@��j@��
@�dZ@�33@��y@�E�@��-@��@�&�@��@�r�@�A�@��@��
@�+@�X@�1'@�@��#@�O�@���@��/@�V@��/@�  @���@�"�@��R@��@�`B@�?}@�V@��9@�b@��@��\@�@�bN@��@�%@�j@��
@��w@�ƨ@��
@��P@�E�@���@�dZ@��H@���@���@�n�@�J@�V@��u@�Z@�9X@�A�@�bN@���@��`@�O�@���@��#@��@��@�ff@�1@�S�@��@��-@��#@�O�@�j@��m@��@�C�@���@��@�dZ@�V@�@��@��/@��/@�Ĝ@�z�@�A�@��m@��@�\)@��y@��+@���@�/@�G�@�/@��@��9@�z�@�9X@��m@���@�K�@���@���@���@�v�@�=q@���@��#@�@�@�=q@���@��!@��+@��!@��@�
=@�
=@���@�-@�?}@��9@��9@��j@���@��j@�1'@���@��P@�l�@�o@���@��+@�=q@��#@�`B@��@��`@���@�A�@��@�1@��m@��
@���@�K�@�@��@��y@��H@��@��@��@���@���@�v�@�E�@�-@��@��-@�O�@�&�@�%@��@�Z@� �@��@�ƨ@���@�S�@���@���@�n�@�E�@�-@�{@��T@���@�O�@�\)@{�F@r=q@iG�@b�@W��@SdZ@K��@E��@>E�@5�h@-�@(�u@$9X@ff@%@�@�9@;d@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aǟ�Aǥ�Aǩ�Aǧ�AǬAǮAǥ�AǕ�AǗ�AǙ�Aǥ�Aǧ�Aǩ�Aǰ!AǴ9AǶFAǰ!AǮAǝ�A�p�A�`BA�`BA�bNA�`BA�^5A�ZA�VA�S�A�O�A�E�A�33A��Aŗ�A��TA�+A�^5A���A���A�I�A���A�ĜA�z�A�+A��mA��A��jA��uA���A�E�A�  A��RA��A�A�A��A��mA��A�r�A���A�E�A��mA�v�A��A��9A�JA��yA��A��A�ffA���A��!A��A��A�&�A��TA��FA�dZA��A��uA��A���A��PA�n�A�VA�K�A��A���A���A��A���A��`A���A�
=A���A��A�l�A�A�hsA��!A��A�5?A�JA��uA���A���A�%A��A��7A�I�A�VA��PA���A��A���A��A��hA�XA���A���A�G�A���A�l�A���A��A�r�A�Q�A�v�A�~�A��A�%A�A�K�A{G�Ay�;Ax��Av�+Aq�AnbAlE�Ak�AjVAi�FAhz�AgO�Af9XAc�hAa�A`�A^��A]��A\v�AZ�AX��AWO�AU�wAS�-AQ�AP �AN�/AM�ALffAK�AIG�AF��AE"�AB{A?�A<�+A;&�A:{A9K�A8bNA7oA4�A2��A1��A1%A0 �A.�9A-�7A,��A,A*�DA)�A(�RA&�yA%��A$ffA#ƨA"�yA"$�A!p�A �A?}A=qAx�A�RA{Ar�A��A�A(�A��A�^A�AA�AC�A^5A�A7LA$�A��AA�A�uA��Ap�A?}A
��A
�A	"�A�^AI�A�9AhsAȴA�A �u@�M�@�G�@��@�l�@�&�@��m@�
=@�{@��7@���@�1@���@���@���@�@�ƨ@��@畁@�5?@�h@�V@�33@�@�Z@߅@��y@�5?@݉7@�&�@ܛ�@۝�@��@��@ա�@���@���@��@θR@�ȴ@�|�@� �@�r�@Л�@мj@�&�@�Q�@͑h@�ff@ɺ^@�X@��`@�j@ǶF@�\)@��@Ɨ�@�V@��@�/@��@Õ�@��y@�ff@��T@�`B@���@���@��@�v�@�$�@��@��T@���@�@�@���@��@�  @�|�@�"�@���@��@���@��@�O�@��j@��
@�dZ@�33@��y@�E�@��-@��@�&�@��@�r�@�A�@��@��
@�+@�X@�1'@�@��#@�O�@���@��/@�V@��/@�  @���@�"�@��R@��@�`B@�?}@�V@��9@�b@��@��\@�@�bN@��@�%@�j@��
@��w@�ƨ@��
@��P@�E�@���@�dZ@��H@���@���@�n�@�J@�V@��u@�Z@�9X@�A�@�bN@���@��`@�O�@���@��#@��@��@�ff@�1@�S�@��@��-@��#@�O�@�j@��m@��@�C�@���@��@�dZ@�V@�@��@��/@��/@�Ĝ@�z�@�A�@��m@��@�\)@��y@��+@���@�/@�G�@�/@��@��9@�z�@�9X@��m@���@�K�@���@���@���@�v�@�=q@���@��#@�@�@�=q@���@��!@��+@��!@��@�
=@�
=@���@�-@�?}@��9@��9@��j@���@��j@�1'@���@��P@�l�@�o@���@��+@�=q@��#@�`B@��@��`@���@�A�@��@�1@��m@��
@���@�K�@�@��@��y@��H@��@��@��@���@���@�v�@�E�@�-@��@��-@�O�@�&�@�%@��@�Z@� �@��@�ƨ@���@�S�@���@���@�n�@�E�@�-@�{@��T@���@�O�@�\)@{�F@r=q@iG�@b�@W��@SdZ@K��@E��@>E�@5�h@-�@(�u@$9X@ff@%@�@�9@;d@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuBoBoBoBoBoBoBuBuBuBuBoBoBoBoBoBoBoBbBPBPBPBPBJBJBJBJBDBDB	7B+B%B
��B
�sB
�;B
�/B
�ZB
�B
�sB
�B
�B
��B
=BBB�B.B1'B>wBL�B\)Bw�B��B��B�#B�yB��BVB&�B8RBL�BM�B8RB7LBF�B_;BhsBq�Bt�Bv�By�B�B�PB�oB�oB�\B�VB�VB�DB�JB�uB��B��B��B�7B� Bx�Bp�BgmBbNB`BBbNBcTBaHBYBS�BL�BB�B@�B7LB'�B&�B"�B�B%B��B�B�fB�BB�B��BŢB�qB�B��B�Bp�B[#B:^BoB
��B
�ZB
�B
��B
�dB
��B
�%B
l�B
R�B
7LB
{B	�sB	�B	ȴB	�3B	�{B	x�B	k�B	e`B	ffB	ffB	bNB	\)B	R�B	F�B	C�B	>wB	7LB	2-B	,B	'�B	�B	�B	uB	
=B	  B��B��B�B�mB�;B�B��B��B�3B�B��B��B��B��B��B��B��B�hB�VB�=B�DB�=B�PB�DB�=B�DB�1B�%B�B�7B�B�B�%B�+B�1B�7B�B�+B�B�B�B}�Bw�Bv�Bv�Bs�Bp�Bo�Bn�BjBffBdZBbNBcTBcTBaHB`BB_;B^5B\)BZBYBT�BT�BP�BK�BL�BL�BK�BK�BH�BE�BE�BI�BH�BF�BE�BD�BF�BE�BJ�BI�BF�BE�BC�BC�BD�BA�BC�BD�BD�BL�BO�BR�BVBZB^5B_;B`BB_;B^5B[#B\)B_;B]/B\)B^5BcTB`BBe`Bq�B{�B� B�%B�JB��B��B��B�{B�oB�bB�\B�PB�JB�JB�JB�PB�bB�{B��B��B��B��B��B��B��B��B��B��B��B�B�9B�?B�?B�LB�^B�}BĜBŢBŢBŢBƨBȴBɺB��B��B��B��B�
B�B�)B�NB�mB�yB�B��B��B��B��B��B��B	B	B	B	B	B	%B	DB	oB	�B	�B	�B	�B	!�B	$�B	)�B	)�B	,B	,B	-B	-B	+B	'�B	$�B	#�B	$�B	&�B	&�B	'�B	+B	/B	1'B	2-B	.B	/B	0!B	1'B	2-B	2-B	49B	8RB	=qB	A�B	D�B	G�B	K�B	N�B	Q�B	T�B	XB	\)B	^5B	aHB	hsB	y�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�hB	�hB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�jB	�wB	�wB	�}B	��B	B	B	B	B	��B	�wB	�}B	��B	ÖB	ŢB	ÖB	��B	��B	B	ĜB	ÖB	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B
B
PB
�B
 �B
(�B
2-B
6FB
<jB
E�B
N�B
S�B
[#B
dZB
jB
q�B
p�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BuBoBoBoBoBoBuBuBuBuBuBoBoBoBoBoBoBuBhBVBPBPBPBJBJBJBJBDBJBDBJBuB%B
�B
�fB
�HB
�sB
�B
�B
�B
��B
��BVBJB	7B�B5?B6FBA�BO�B_;Bz�B��BĜB�ZB�B��B{B,B>wBT�B[#BA�BC�BR�BffBo�Bv�By�B}�B� B�+B�bB��B��B��B�uB�uB�\B�\B��B��B�B�B�\B�%B� Bx�Bq�Bp�BffBgmBl�BjB`BB\)BVBM�BK�BD�B1'B33B5?B,BhB��B�B�B�sB�NB�
B��B��B�}B��B�VB�Bo�BO�B!�B  B
�B
�`B
�5B
�
B
�jB
��B
~�B
s�B
W
B
2-B	�B	�fB	�#B	��B	��B	�B	s�B	k�B	l�B	o�B	k�B	gmB	cTB	T�B	M�B	F�B	A�B	<jB	8RB	6FB	+B	%�B	!�B	�B	
=B	B��B��B�B�B�mB�#B�BƨB�wB�-B�B��B�B�B�B��B��B�{B�oB��B�oB�{B�oB�{B�oB�uB�uB�\B�hB�DB�=B�JB�VB�bB�oB�PB�PB�7B�7B�PB�B|�B~�B�Bz�Bw�Bu�Bu�Bq�Bl�BjBk�Bl�Bk�BgmBdZBdZBbNB_;B`BB_;B^5BaHB]/BXBVBR�BS�BVBQ�BI�BI�BO�BP�BK�BI�BH�BI�BI�BO�BQ�BN�BL�BH�BH�BL�BI�BH�BG�BH�BS�BVBXBYB]/BaHBbNBbNBcTBdZBcTBffBe`Be`BcTBbNBiyB_;Be`Bo�Bz�B� B�%B�PB��B��B��B��B�{B�bB�hB�bB�VB�VB�VB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�?B�FB�LB�dBBǮBŢBǮBǮBƨBɺB��B��B��B��B�B�
B�#B�)B�ZB�sB�B�B��B��B��B��B��B��B	%B	B	%B	B	B	+B	DB	uB	�B	�B	�B	 �B	!�B	&�B	+B	+B	.B	,B	0!B	0!B	/B	'�B	,B	#�B	$�B	&�B	&�B	'�B	+B	1'B	6FB	7LB	2-B	1'B	0!B	1'B	2-B	2-B	7LB	:^B	>wB	A�B	D�B	G�B	K�B	N�B	P�B	S�B	XB	\)B	^5B	_;B	hsB	p�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�qB	�}B	�wB	�}B	��B	B	ÖB	B	ŢB	B	�wB	�}B	��B	ĜB	ǮB	ŢB	B	��B	ÖB	ŢB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�;B	�;B	�;B	�HB	�HB	�HB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
PB
�B
 �B
(�B
2-B
6FB
=qB
E�B
N�B
S�B
[#B
e`B
k�B
q�B
p�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�C�<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<D��<#�
<D��<�C�<�t�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<�C�<e`B<T��<�o<��
<�1<u<49X<#�
<D��<�t�<���<���<�1<�t�=o<��<�h<49X<D��<�t�<�h<�1<49X<#�
<#�
<#�
<#�
<#�
<49X<�o<T��<#�
<#�
<#�
<#�
<D��<T��<D��<D��<e`B<T��<#�
<#�
<#�
<#�
<#�
<e`B<�C�<e`B<��
<���<�o<#�
<#�
<#�
<#�
<D��<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452052012011014520520120110145205  AO  ARGQ                                                                        20111130143838  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143838  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145205  IP                  G�O�G�O�G�O�                