CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   +   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-11-21T18:58:25Z creation;2009-07-21T01:35:22Z update;2015-06-12T06:44:49Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         :   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          1l   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    1|   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    1�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    1�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    1�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  1�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  1�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                     2   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        2<   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    2@   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    2D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    2H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    2X   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     2h   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    2�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    2�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     2�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     2�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     2�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    2�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           2�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    2�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        3    LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           3   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           3   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    3   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    3   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    3$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    3(   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    3,   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        4,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z         �  40   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  4�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure        �  5   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  5�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  5�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature         �  6�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  78   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature         �  7d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  8<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                     8�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    9   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ;   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    =   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                    ?   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ?$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ?(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ?,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ?0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ?4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ?t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ?�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ?�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ?�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ?�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ?�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ?�Argo profile    3.1 1.2 19500101000000  29028   SAGE                                                            Nobuyuki SHIKAMA                                                PRES            TEMP              A   JA  20051121185825  20150621024518  T3_25338_273                    2C  D   APEX                            77                              091099                          845 @��g�s��1   @��hI��'@DkƧ�c��E��1   ARGOS   A   A   Primary sampling: discrete [The pressure is measured every 6 seconds]                                                                                                                                                                                              @�  A   Ap  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cŀ Cʀ 1111111111111111111111111111111111111111111 @���A<��A�ffA�ffA�ffB33B;33Bc33B���B���B���B���Bՙ�B陚B���C��C��C��C&��C0��C:��CD��CN��CX��Cb��Cl��Cv��C�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ffC�ff1111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��As�As�mAs��At1At1At1As�Ap  A;�A5�^A,=qA&�/A!�PA!��A"{A#/A#
=A!l�A �`A   Ap�A��A�A~�A�A��AVAhsAn�An�AE�A�/A1'A$�A�HA ȴ@��@���@�p�@�\@���@�l�@�331111111111111111111111111111111111111111111 As�As�mAs��At1At1At1As�Ap  A;�A5�^A,=qA&�/A!�PA!��A"{A#/A#
=A!l�A �`A   Ap�A��A�A~�A�A��AVAhsAn�An�AE�A�/A1'A$�A�HA ȴ@��@���@�p�@�\@���@�l�@�331111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PRES_ADJ=PRES-SP(NextCycle),  where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                      TEMP_ADJ=TEMP                                                                                                                                                                                                                                                   SP(NextCycle)=-1.8(dbar)                                                                                                                                                                                                                                        None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              2009071307341420090713073414JA  ARFMfmtp2.2                                                                 20051121185825  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051121185825  QCP$                G�O�G�O�G�O�            BB7CJA  ARGQrqcp2.3                                                                 20051121185825  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20051122010629                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060609070504  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060609070505  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060609070505  QCP$                G�O�G�O�G�O�           176BCJA  ARUP                                                                        20060609090441                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060623042235  QCP$                G�O�G�O�G�O�            BB7CJA  ARGQaqcp2.5                                                                 20060623042235  QCP$                G�O�G�O�G�O�            BB40JA  ARUP                                                                        20060623045955                      G�O�G�O�G�O�                JA  ARFMdecpT3_b                                                                20090311052403  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090311060112  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090311060112  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090311060113  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090311060114  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090311060114  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090311060114  QCP$                G�O�G�O�G�O�            BB40JA  ARGQaqcp2.8b                                                                20090311060114  QCP$                G�O�G�O�G�O�            BB40JA  ARGQrqcpt16b                                                                20090311060114  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20090311071432                      G�O�G�O�G�O�                JM  ARCAJMQC1.0m                                                                20090713073414  CV  PRES            G�O�G�O�G�O�                JM  ARSQJMQC1.0m                                                                20090713073414  IP                  G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090721013130  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090721013522                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150612064447                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621024518                      G�O�G�O�G�O�                