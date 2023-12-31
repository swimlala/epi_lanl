CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  <   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:21:10Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        &�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  k�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     &�  u�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     &�  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &�  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� $<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� -�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� T�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� {�   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� �D   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� ܘ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &�    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 3�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� =�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � dp   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   e0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   q0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �TArgo profile    3.1 1.2 19500101000000  20181120212110  20200901153529  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               G   G   GAAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @��O�!$@��O�!$@��O�!$111 @��P\�,,@��P\�,,@��P\�,,@68���F@68���F@68���F�cyp��
=�cyp��
=�cyp��
=111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D y�D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�qD�/\D��D���D��D�:�D��RD��{D�
D�H�D��)D���D��D�:�Dڊ�D�� D���D�33D�`�D�${G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���                                >L��        =���>L��    =���            >L��=���    =���        =���        =���        >���>���=���        >L��=���                        =���    =���>���=���    >L��        =���                                                        =���>L��                        =���=���    =���>L��    =���>L��        >L��=���                =���>L��                    >���?   =���    =���=���        =���            >L��    =���        >L��>���>L��        >L��=���=���>L��=���        =���>���>L��            =���        =���    =���    =���>L��=���        >L��=���=���=���    =���>���>L��    =���=���    =���=���=���=���=���=���>L��    =���>���=���=���>L��>L��=���    >L��>L��>L��>���=���>L��>L��>���>���>���>L��>L��>���>L��=���>���>���>���>���>L��>���>L��>���>L��>���>���>L��>���>���>L��>���>L��>���>���>L��>L��>L��>���>L��>���>L��>���>L��>L��>���>���>���>L��>L��>L��>���>���>L��>L��>���>L��>L��>���>���>���>���>���>���>���>���>���>���>L��>���>���>���=���>���>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>L��>���?   >���>���>L��>L��>���>���>���>���>���>���=���>���>���>���>���>L��>���>���>���>���>���>L��>L��>L��>���>���>L��>���>���>���>���>���>���>L��>���>L��>L��>���>���>���>���>���>L��>L��>���>L��>���>���>L��>���>���>���>L��>L��>L��>���>���>���>���>���>���>���>L��>���>���>���>���>L��>���>L��>���>���>L��>L��>���>���>L��>L��>���>���>L��>���>���>���>L��>���>���>���>L��>���>L��>���>L��>���>L��>���>L��>���>���>���>���>���>���?   >���>���>L��>L��>L��>L��>���>L��>���>L��>L��>���>L��>���>���>���>���>L��>���>L��>L��>L��>L��>���=���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>L��>���>L��>L��>���>���>���>L��>���>L��>���>L��>L��>���>���>���>���>���>L��>���>���>���>L��>L��=���>L��>���>���>���>���>���>���>���>���>���>���>L��>L��=���>L��=���>L��>L��>L��>���>���?   ?   ?��?333?333?fff?fff?�  ?���?���?���?���?�33?�  ?�  ?���?ٙ�?�ff?�33?�33@   @ff@��@33@��@��@   @&ff@,��@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�  @�ff@���@���@�  @�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A33AffA  A	��A33A��AffA��A33A��A  A��A33A��AffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��ANffAP  AQ��AS33AT��AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��Ah  Ah  Ak33Al��AnffAp  Aq��As33As33AvffAx  Ax  A{33A|��A~ffA�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A���A�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�ffA�  A���A͙�A�ffA�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�33Do��Do��Dp  Dp�Dp3Dp  Dp&fDp33Dp9�DpFfDpS3DpY�DpffDpl�Dpy�Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��DpٚDp� Dp��Dp��Dq  Dq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqy�Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq�fDq��Dq��Dr  Dr�Dr3Dr  Dr&fDr33Dr9�DrFfDrL�DrY�DrffDrl�Dry�Dr� Dr��Dr�3Dr� Dr�fDr�3Dr��Dr�fDr��DrٚDr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds��Ds�3Ds� Ds�fDs�3DsٚDs�fDs�3Ds��DtfDt�Dt�Dt  Dt,�Dt33Dt@ DtL�DtS3Dt` DtffDts3Dty�Dt�f@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�  @�ff@���@���@�  @�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A33AffA  A	��A33A��AffA��A33A��A  A��A33A��AffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��ANffAP  AQ��AS33AT��AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��Ah  Ah  Ak33Al��AnffAp  Aq��As33As33AvffAx  Ax  A{33A|��A~ffA�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A���A�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�ffA�  A���A͙�A�ffA�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�33Do��Do��Dp  Dp�Dp3Dp  Dp&fDp33Dp9�DpFfDpS3DpY�DpffDpl�Dpy�Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��DpٚDp� Dp��Dp��Dq  Dq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqy�Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq�fDq��Dq��Dr  Dr�Dr3Dr  Dr&fDr33Dr9�DrFfDrL�DrY�DrffDrl�Dry�Dr� Dr��Dr�3Dr� Dr�fDr�3Dr��Dr�fDr��DrٚDr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds��Ds�3Ds� Ds�fDs�3DsٚDs�fDs�3Ds��DtfDt�Dt�Dt  Dt,�Dt33Dt@ DtL�DtS3Dt` DtffDts3Dty�Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@1�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A��
A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D q�D �RDxRD�RDq�D�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo��DpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDt^�Dyz>D�	�D�+�D��HD�� D� D�6�D��{D�ȤD�3D�ED��RDǽD���D�6�Dچ�D��)D���D�/\D�]D� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����	����������������=���������	=�������	������=�����	�����	�������	�������	����>8Q�>8Q켣�	����=�����	���������������	�����	>8Q켣�	��=���������	�������������������������������	=�����������������	���	�����	=�������	=������=�����	�����������	=������������>8Q�>\���	�����	���	�������	������=�������	����=��>8Q�=������=�����	���	=�����	�������	>8Q�=�����������	�������	�����	�����	=�����	����=�����	���	���	�����	>8Q�=�������	���	�����	���	���	���	���	���	=�������	>8Q켣�	���	=��=�����	��=��=��=��>8Q켣�	=��=��>8Q�>8Q�>�\)=��=��>8Q�=�����	>�\)>�\)>8Q�>8Q�=��>8Q�=��>8Q�=��>�\)>�\)=��>8Q�>8Q�=��>8Q�=��>8Q�>8Q�=��=��=��>8Q�=��>8Q�=��>8Q�=��=��>�\)>8Q�>8Q�=��=��=��>�\)>8Q�=��=��>8Q�=��=��>�\)>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q�>8Q�=��>8Q�>8Q�>8Q켣�	>8Q�=��>8Q�>�\)>8Q�=��>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>8Q�=��>�\)>\>8Q�>�\)=��=��>8Q�>8Q�>�\)>8Q�>�\)>8Q켣�	>8Q�>8Q�>8Q�>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�=��=��=��>8Q�>8Q�=��>8Q�>8Q�>8Q�>8Q�>�\)>8Q�=��>8Q�=��=��>8Q�>8Q�>8Q�>�\)>8Q�=��=��>8Q�=��>8Q�>�\)=��>8Q�>8Q�>8Q�=��=��=��>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��>�\)>�\)>8Q�>8Q�=��>8Q�=��>8Q�>8Q�=��=��>8Q�>8Q�=��=��>8Q�>8Q�=��>8Q�>8Q�>8Q�=��>�\)>8Q�>8Q�=��>8Q�=��>8Q�=��>8Q�=��>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>\>�\)>8Q�=��=��=��=��>8Q�=��>8Q�=��=��>8Q�=��>�\)>8Q�>�\)>8Q�=��>8Q�=��=��=��=��>8Q켣�	>8Q�>8Q�>�\)=��>8Q�>8Q�>�\)>8Q�>8Q�>8Q�=��>8Q�>8Q�=��>8Q�=��=��>8Q�>8Q�>8Q�=��>8Q�=��>8Q�=��=��>8Q�>8Q�>8Q�>�\)>8Q�=��>8Q�>8Q�>8Q�=��=�����	=��>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q�>8Q�>8Q�>8Q�=��=�����	=�����	=��=��=��>8Q�>�\)>\>\>�?z�?z�?G�?G�?aG�?z�H?z�H?�=q?�=q?��
?���?���?�p�?�=q?�
=?��
?��
?��?�p�@�@�@�@�@Q�@�R@%�@+�@1�@>�R@E�@K�@Q�@^�R@e�@k�@xQ�@~�R@��\@���@�(�@�\)@��\@���@�(�@��\@�@���@�(�@��\@�@���@�(�@\@�@���@�(�@ҏ\@�@���@�(�@�\)@�\@���@�(�@�\)@�\@�@�(�@�\*AG�AG�Az�A{A�A	G�A
�HAz�A�AG�A�HA{A�AG�A�HAz�A{A�A"�HA$z�A&{A'�A)G�A*�HA,z�A.{A1G�A2�HA4z�A6{A7�A9G�A:�HA>{A?�AAG�AB�HADz�AG�AIG�AJ�HALz�AN{AO�AQG�AR�HAV{AW�AYG�AZ�HA\z�A^{A_�AaG�Ab�HAf{Af{AiG�Aj�HAlz�An{Ao�AqG�AqG�Atz�Av{Av{AyG�Az�HA|z�A~{A�A�A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A��
A���A�=pA�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�=pA�
=A��
AУ�A�=pA�
=A��
Aԣ�A�p�A�
=A��
Aأ�A�p�A�=pA�
=A��
Aܣ�A�=pDo�Do��Do�RDpDp�DpRDp�Dp+�Dp1�Dp>�DpK�DpQ�Dp^�DpeDpq�DpxRDp�Dp��Dp�RDp��Dp��Dp��Dp��Dp�Dp��Dp�RDp�Dp��Dp�RDqDq�DqRDq�Dq+�Dq1�Dq>�DqEDqQ�DqXRDqeDqq�DqxRDq�Dq��Dq�RDq��Dq��Dq��Dq��Dq�Dq��Dq޸Dq�Dq��Dq�RDrDr�DrRDr�Dr+�Dr1�Dr>�DrEDrQ�Dr^�DreDrq�DrxRDr�Dr��Dr�RDr��Dr��Dr��Dr��Dr�Dr��Dr޸Dr�Dr��Dr�RDsDs�DsRDs�Ds+�Ds8RDs>�DsK�DsQ�Ds^�DseDsq�DsxRDs�Ds��Ds�RDs�Ds��Ds�RDs��Ds˅Ds��Ds޸Ds�Ds��Ds��DtDt�DtRDt%Dt+�Dt8RDtEDtK�DtXRDt^�Dtk�Dtq�Dt~�@+�@1�@>�R@E�@K�@Q�@^�R@e�@k�@xQ�@~�R@��\@���@�(�@�\)@��\@���@�(�@��\@�@���@�(�@��\@�@���@�(�@\@�@���@�(�@ҏ\@�@���@�(�@�\)@�\@���@�(�@�\)@�\@�@�(�@�\*AG�AG�Az�A{A�A	G�A
�HAz�A�AG�A�HA{A�AG�A�HAz�A{A�A"�HA$z�A&{A'�A)G�A*�HA,z�A.{A1G�A2�HA4z�A6{A7�A9G�A:�HA>{A?�AAG�AB�HADz�AG�AIG�AJ�HALz�AN{AO�AQG�AR�HAV{AW�AYG�AZ�HA\z�A^{A_�AaG�Ab�HAf{Af{AiG�Aj�HAlz�An{Ao�AqG�AqG�Atz�Av{Av{AyG�Az�HA|z�A~{A�A�A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A��
A���A�=pA�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aȣ�A�p�A�
=A��
Ạ�A�p�A�=pA�
=A��
AУ�A�=pA�
=A��
Aԣ�A�p�A�
=A��
Aأ�A�p�A�=pA�
=A��
Aܣ�A�=pDo�Do��Do�RDpDp�DpRDp�Dp+�Dp1�Dp>�DpK�DpQ�Dp^�DpeDpq�DpxRDp�Dp��Dp�RDp��Dp��Dp��Dp��Dp�Dp��Dp�RDp�Dp��Dp�RDqDq�DqRDq�Dq+�Dq1�Dq>�DqEDqQ�DqXRDqeDqq�DqxRDq�Dq��Dq�RDq��Dq��Dq��Dq��Dq�Dq��Dq޸Dq�Dq��Dq�RDrDr�DrRDr�Dr+�Dr1�Dr>�DrEDrQ�Dr^�DreDrq�DrxRDr�Dr��Dr�RDr��Dr��Dr��Dr��Dr�Dr��Dr޸Dr�Dr��Dr�RDsDs�DsRDs�Ds+�Ds8RDs>�DsK�DsQ�Ds^�DseDsq�DsxRDs�Ds��Ds�RDs�Ds��Ds�RDs��Ds˅Ds��Ds޸Ds�Ds��Ds��DtDt�DtRDt%Dt+�Dt8RDtEDtK�DtXRDt^�Dtk�Dtq�Dt~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AüjAú^AøRAþwA���A�ƨA�ĜA�ĜA�ƨA���A�ĜAÙ�A�\)A�M�A�;dA��A£�A��A�K�A�33A���A��A���A��A�
=A���A�M�A��7A�bA�A�z�A�7LA�(�A��A�JA�%A��A��wA�1A�ƨA�ffA�-A�XA�bA��A�oA��FA���A��RA��A�
=A�E�A�x�A�A��RA�^5A�&�A��`A�ĜA�v�A�$�A���A�7LA��!A���A�`BA��7A��
A�7LA�jA���A���A��A���A��jA��yA��\A�5?A���A��A��A��A�{A��DA���A��-A�A�ffA��^A�/A��;A�{A�(�A��`A��A�r�A�bNA���A�?}A�n�A��hA�/A� �A���A��\A�VA���A�O�A��wA�?}A��+A�A�A���A�x�A�E�A�1'A���A��A��PA�5?A��mA��hA~-A}�A{�hAz�Aw�AtVAs�As�Aq�PAo�hAn��Am?}AkC�Ah9XAd=qA`~�A]l�AZ��AX��AV��AU�hATJARbNAP��AOt�ANbNAMl�AK�#AJ��AH��AGoAFv�AF�AEx�AE"�AD�9AD�\ADz�AC�7AB�HAA��A@�DA?A?"�A=t�A<ĜA<bA;VA:1A9&�A7�TA6��A5�A2��A/��A/`BA-�A+�FA*^5A(ffA&�uA%��A$��A#�A#C�A!�A �9AK�A9XA�`A�Az�A�^A&�A�A�RAA/AhsAoA��A�A�RA$�A�A��AffA~�A��A`BA�HA�A
�A
ffA	�^A	�A��AA;dA�RA�mA�A~�A"�A �!A 9X@�|�@���@��#@�&�@�I�@�|�@�{@��@�X@�bN@�(�@�  @�@�~�@���@�1@�\)@�{@���@�\@�@蛦@�I�@��;@畁@��@旍@���@�1'@�|�@��@�n�@�`B@�9@�1@�G�@�A�@���@ڰ!@�E�@�{@���@ٙ�@���@׶F@��H@��@��#@�p�@���@Ӿw@ӍP@���@Ѻ^@� �@�l�@Ώ\@���@���@͡�@�V@˥�@��#@�|�@�n�@�E�@�V@� �@Ý�@���@�/@��w@�=q@�7L@�7L@��7@��@�Ĝ@�Q�@���@��R@�E�@��@��@��h@�p�@�X@�`B@�`B@�`B@�`B@�7L@�V@���@�Q�@��w@��@�G�@��u@�A�@��;@�t�@���@�@���@���@��h@��@�`B@�/@��@�r�@��;@�dZ@��\@��@�o@��F@��m@�|�@�K�@�"�@�
=@���@�$�@�M�@�=q@��^@�p�@�V@�bN@�Q�@��@�|�@�dZ@�;d@��@�
=@�S�@�;d@�~�@��-@��h@���@���@���@�hs@��@�r�@�j@�r�@��y@���@�+@�V@���@���@�p�@��h@���@��@�@���@���@��7@��7@�hs@�p�@�O�@�7L@�V@��j@��D@��;@�;d@�o@�J@�hs@��j@�1'@��@��m@��P@��@�
=@���@���@�n�@�^5@�n�@��@���@��j@�x�@�V@���@�Q�@�9X@�9X@�I�@�j@��@���@���@�@��@���@�^5@�=q@�@�hs@�7L@��@��@���@��u@�Z@�1@�  @��m@���@�K�@��y@�E�@��-@��@��@��@��/@��@�9X@��
@�J@���@��`@�j@�I�@�t�@�;d@���@�^5@���@��-@��-@�@�x�@�?}@��@���@�  @��@�1'@� �@��@�Z@�r�@��w@��\@�v�@�=q@���@���@�@|M@t�@k�]@c�0@Z
�@P�@H9X@Azx@9�@3{J@/ƨ@,K^@&R�@"L0@�@��@�@o�@S�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��wA���A��A�VA�$�A��+A�$�A�ƨA�I�A��
A�ƨAÃA���A��A�jA�  A��A��#A�r�A���A���A��TA���A���A�A�A��7A�JA���A�dZA�AÕ�A�ffA�%A��7A�~�A�z�A�1A���A��A�ZA��!A�9XA�ĜA�%A��TAÝ�AÏ\A�A�A��A��jA��-A�$�A�I�A��jA��
A��A�=qA�~�A��7A��hA�|�A��A�jA��A�`BA�K�A��\A�z�A���A���A�VA�^5A���A�bA�v�A�x�A��+A���AÏ\A���A�AÃA�S�A���A���A�|�A�\)A�v�A���A��7A�bNA�~�A��9A��DA�O�A�(�A�Q�A��
AÙ�AÇ+A�33A�7LAÅA��
A��\A��#A�1A�M�A�l�A��A��+A��;A��A���A�9XAÑhA�dZA��/A��DA�ƨAÃA�7LAÇ+A�^5A� �A�?}A��TAËDAÇ+A��A��DA�A�A���A��A�C�A�r�A�dZA��mA��+A��AÉ7AÙ�A�A�jAÛ�A�`BA�r�A��A�bA���Aã�Aß�A���A�z�AËDA��hA��TAå�A��
A×�AÝ�A¾wAá�A�bNA�9XAå�Aã�Aå�Aá�Aß�A�-A��A��Aå�Aã�Aã�A�Q�A�  Aç�Aá�AÝ�Aá�Aá�Aß�Aß�AìAÕ�Aå�AÝ�Aß�AÝ�Aß�AÛ�Aå�Aã�AÝ�AÛ�Aß�AÝ�AÓuAÝ�AÛ�AÛ�A×�Aß�AÛ�AÛ�AÛ�AÛ�AÝ�AÝ�AÛ�Aß�AÝ�AÝ�AÙ�Aá�AÝ�AÛ�AÝ�Aá�AÙ�AÙ�AÝ�AÕ�AÛ�AÛ�AÛ�AÕ�AÓuAÛ�A×�AÝ�AÛ�AÛ�AÝ�Aß�AÝ�AÝ�AÝ�AÛ�Aß�AÛ�A×�AÏ\AÙ�AÙ�AÛ�AÝ�Aß�AÛ�Aß�AÝ�AÙ�Aß�Aã�Aß�AÝ�AÝ�Aß�AÝ�AÙ�AÝ�Aß�Aß�AÛ�AÛ�AÙ�AÛ�AÝ�Aã�A×�AÙ�Aß�AÙ�A×�AÝ�AÓuAÙ�AÓuAÙ�AÓuAÑhAÓuAÕ�AÕ�AËDAÕ�AÛ�AÛ�AÑhAÓuAÍPAËDAÕ�AÇ+AÕ�AÕ�A�hsAÓuAÑhAÏ\AÑhAÕ�AÕ�AÑhAÓuAÓuAÙ�AÓuAÓuAÓuAÓuAÓuAÓuAÏ\AÃAÓuAÓuAÛ�AÇ+AÍPAËDAÓuA×�AÕ�AÛ�AÛ�AÛ�AÙ�AÙ�AÛ�AÙ�AÙ�Aß�AÝ�Aß�Aã�AÝ�Aß�Aã�Aá�Aç�AîAá�AÝ�Aå�Aç�Aå�Aá�Aß�Aá�Aá�Aß�AÙ�AÝ�AÛ�Aã�Aç�Aá�AÓuA×�Aá�Aå�AÙ�Aß�Aá�AÛ�A×�AÑhA×�A��^AÛ�Aá�Aá�Aá�Aá�AÏ\Aá�Aã�Aå�Aå�Aå�Aã�Aá�Aß�Aá�Aã�Aá�AÝ�Aã�Aç�Aé�Aç�Aã�Aã�AìAß�AìAé�Aé�A×�Aã�AîAé�Aé�Aç�Aã�AîAìAé�Aé�AìAå�AîAð!Aé�Aå�Að!Aò-AîAìAîAå�Aò-AîAé�AÓuAã�Aã�Aô9Að!Aé�AöFAöFAöFAöFAöFAöFAò-Aá�Aç�A�v�AìAò-AöFAô9AøRAøRAöFAú^Aú^AöFAú^Aú^AøRAöFAöFAöFAöFAò-Aò-Að!Aò-Aô9AöFAöFAøRAöFAøRAú^Aú^Aú^AøRAú^Aú^AøRAøRAú^AøRAøRAú^AüjAú^Aú^AüjAú^Aú^Aú^AøRAøRAøRAöFAøRAöFAöFAöFAöFAöFAô9AöFAô9AöFAöFAöFAøRAøRAøRAöFAøRAøRAú^Aú^AüjAüjAþwAüjAüjAú^Aú^Aú^AüjAüjAüjAú^Aú^AüjA�AþwAú^A�ƨA�ȴA�ƨA�ƨA�ȴA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA���A�ƨA�ĜA�ƨA���A���AþwAþwAþwA���A�A�A�A�A�A�ȴA�ȴA���A�A�A���A���A���AþwAþwAþwA���AþwAþwAþwA���A���A���AþwAþwAþwAþwAþwA�A�A�A�A�A�ĜA�A�A�A�A�ĜA�AþwAþwAþwA�A�ĜA�A�A�A�A�A�ĜA�A�ĜA�ƨA�ƨA�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ƨA���A�ȴA�ȴA�ȴA�ȴA�ȴA���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A�ȴA�A���AøRAöFAöFAöFAð!Aò-AöFAú^AüjAú^Aò-Aò-AîAîAå�Aå�AÛ�AÝ�Aß�Aá�AÙ�AÓuAÕ�AÓuAÍPAÃA�t�A�t�A�p�A�jA�dZA�\)A�XA�ZA�XA�ZA�XA�XA�VA�VA�VA�Q�A�O�A�O�A�O�A�M�A�O�A�K�A�M�A�K�A�K�A�K�A�K�A�K�@�(�@�1'@�1'@�(�@�1'@�1'@��@� �@�  @�b@� �@��@� �@� �@��@� �@��@��@�b@�1'@�(�@� �@��@�1@�b@�b@�1@�1@�  @�b@��@�1@�b@��@��@�1'@�9X@�9X@�A�@�Z@�Z@�Z@�Z@�Q�@�z�@�r�@�z�@�r�@�r�@�z�@�r�@�z�@�z�@�r�@�r�@�z�@�r�@�j@�j@�Z@�1'@�1'@��@�b@�1@��
@��
@�ƨ@��@���@��P@�+@��@�33@��y@�ȴ@��@��\@�~�@�ff@�^5@�V@�^5@�^5@�ff@�ff@�^5@�^5@�^5@�ff@�n�@�n�@�n�@�v�@�v�@�v�@�~�@�v�@�~�@�v�@�v�@�v�@�v�@�^5@�V@�M�@�=q@�-@�J@��@��T@��T@��T@��#@��#@���@���@���@�@���Aú^AþwAüjAüjAþwAüjAüjAú^AüjAú^Aú^Aú^AøRAøRAøRAú^Aú^AöFAøRAú^AöFAøRAöFAøRAøRAøRAú^Aú^Aú^Aú^Aò-AøRAú^AöFAüjAú^AþwAþwAþwAüjAüjAþwAþwAüjAú^AøRAüjAú^AþwA�ĜA�A�ȴA�ĜA���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�A�ĜA�A�A�A�A�A�ĜA�ĜA�A�ĜA�ƨA�ȴA�ƨA�ĜA�ƨA�A�A�A�A�A���A���A���A�A�A�A�A���A�A���AþwA���A���A�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�A���A�A�A�ĜA�A�ĜA�ƨA�ƨA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ȴA���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AöFAú^AöFAöFAú^AþwA���A���AöFAüjAð!Að!AîAé�Aß�Aß�Aß�Aã�Aã�AÛ�AÓuAÕ�AÑhAÃA�z�A�t�A�x�A�r�A�jA�bNA�^5A�\)A�\)A�\)A�\)A�ZA�XA�XA�ZA�XA�S�A�S�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�@� �@�(�@�1'@�1'@�1'@�9X@�(�@� �@�  @��@� �@� �@� �@�(�@� �@�b@� �@��@��@�1'@�1'@� �@� �@��@��@��@�1@�1@�1@�b@��@�b@�b@��@� �@�1'@�9X@�I�@�I�@�Q�@�bN@�Z@�bN@�bN@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@��@�z�@�r�@�z�@�r�@�j@�bN@�I�@�9X@� �@�b@�b@��m@��m@�ƨ@���@��w@��@�K�@�;d@��@��H@��H@��H@���@��\@�n�@�ff@�^5@�ff@�ff@�ff@�n�@�ff@�ff@�ff@�n�@�n�@�n�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�v�@�n�@�^5@�V@�E�@�5?@�{@���@��@��T@��@��T@��#@���@���@���@���@�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999AüjAú^AøRAþwA���A�ƨA�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dG�O�G�O�A��A�K�A�33A���A��A���G�O�A�
=A���G�O�G�O�A�bA�A�z�A�7LA�(�A��A�JA�%A��A��wA�1A�ƨA�ffA�-A�XA�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A�7LA�jA���A���A��A���A��jA��yA��\A�5?A���A��A��A��A�{A��DA���A��-A�A�ffA��^A�/A��;A�{A�(�A��`A��A�r�A�bNA���A�?}A�n�A��hA�/A� �A���A��\A�VA���A�O�A��wA�?}A��+A�A�A���A�x�A�E�A�1'A���A��A��PA�5?A��mA��hA~-A}�A{�hAz�Aw�AtVAs�As�Aq�PAo�hAn��Am?}AkC�Ah9XAd=qA`~�A]l�AZ��AX��AV��AU�hATJARbNAP��AOt�ANbNAMl�AK�#AJ��AH��AGoAFv�AF�AEx�AE"�AD�9AD�\ADz�AC�7AB�HAA��A@�DA?A?"�A=t�A<ĜA<bA;VA:1A9&�A7�TA6��A5�A2��A/��A/`BA-�A+�FA*^5A(ffA&�uA%��A$��A#�A#C�A!�A �9AK�A9XA�`A�Az�A�^A&�A�A�RAA/AhsAoA��A�A�RA$�A�A��AffA~�A��A`BA�HA�A
�A
ffA	�^A	�A��AA;dA�RA�mA�A~�A"�A �!A 9X@�|�@���@��#@�&�@�I�@�|�@�{@��@�X@�bN@�(�@�  @�@�~�@���@�1@�\)@�{@���@�\@�@蛦@�I�@��;@畁@��@旍@���@�1'@�|�@��@�n�@�`B@�9@�1@�G�@�A�@���@ڰ!@�E�@�{@���@ٙ�@���@׶F@��H@��@��#@�p�@���@Ӿw@ӍP@���@Ѻ^@� �@�l�@Ώ\@���@���@͡�@�V@˥�@��#@�|�@�n�@�E�@�V@� �@Ý�@���@�/@��w@�=q@�7L@�7L@��7@��@�Ĝ@�Q�@���@��R@�E�@��@��@��h@�p�@�X@�`B@�`B@�`B@�`B@�7L@�V@���@�Q�@��w@��@�G�@��u@�A�@��;@�t�@���@�@���@���@��h@��@�`B@�/@��@�r�@��;@�dZ@��\@��@�o@��F@��m@�|�@�K�@�"�@�
=@���@�$�@�M�@�=q@��^@�p�@�V@�bN@�Q�@��@�|�@�dZ@�;d@��@�
=@�S�@�;d@�~�@��-@��h@���@���@���@�hs@��@�r�@�j@�r�@��y@���@�+@�V@���@���@�p�@��h@���@��@�@���@���@��7@��7@�hs@�p�@�O�@�7L@�V@��j@��D@��;@�;d@�o@�J@�hs@��j@�1'@��@��m@��P@��@�
=@���@���G�O�@�^5@�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�j@��@���@���@�@��@���@�^5@�=q@�@�hs@�7L@��@��@���@��u@�Z@�1@�  @��m@���@�K�@��y@�E�@��-@��@��@��@��/@��@�9X@��
@�J@���@��`@�j@�I�@�t�@�;d@���@�^5@���@��-@��-@�@�x�@�?}@��@���@�  @��@�1'@� �@��@�Z@�r�@��w@��\@�v�@�=qG�O�@���@�@|M@t�@k�]@c�0@Z
�@P�@H9X@Azx@9�@3{J@/ƨ@,K^@&R�@"L0@�@��@�@o�@S�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��wA���A��A�VA�$�A��+A�$�A�ƨA�I�A��
A�ƨAÃA���A��A�jA�  A��A��#A�r�A���A���A��TA���A���A�A�A��7A�JA���A�dZA�AÕ�A�ffA�%A��7A�~�A�z�A�1A���A��A�ZA��!A�9XA�ĜA�%A��TAÝ�AÏ\A�A�A��A��jA��-A�$�A�I�A��jA��
A��A�=qA�~�A��7A��hA�|�A��A�jA��A�`BA�K�A��\A�z�A���A���A�VA�^5A���A�bA�v�A�x�A��+A���AÏ\A���A�AÃA�S�A���A���A�|�A�\)A�v�A���A��7A�bNA�~�A��9A��DA�O�A�(�A�Q�A��
AÙ�AÇ+A�33A�7LAÅA��
A��\A��#A�1A�M�A�l�A��A��+A��;A��A���A�9XAÑhA�dZA��/A��DA�ƨAÃA�7LAÇ+A�^5A� �A�?}A��TAËDAÇ+A��A��DA�A�A���A��A�C�A�r�A�dZA��mA��+A��AÉ7AÙ�A�A�jAÛ�A�`BA�r�A��A�bA���Aã�Aß�A���A�z�AËDA��hA��TAå�A��
A×�AÝ�A¾wAá�A�bNA�9XAå�Aã�Aå�Aá�Aß�A�-A��A��Aå�Aã�Aã�A�Q�A�  Aç�Aá�AÝ�Aá�Aá�Aß�Aß�AìAÕ�Aå�AÝ�Aß�AÝ�Aß�AÛ�Aå�Aã�AÝ�AÛ�Aß�AÝ�AÓuAÝ�AÛ�AÛ�A×�Aß�AÛ�AÛ�AÛ�AÛ�AÝ�AÝ�AÛ�Aß�AÝ�AÝ�AÙ�Aá�AÝ�AÛ�AÝ�Aá�AÙ�AÙ�AÝ�AÕ�AÛ�AÛ�AÛ�AÕ�AÓuAÛ�A×�AÝ�AÛ�AÛ�AÝ�Aß�AÝ�AÝ�AÝ�AÛ�Aß�AÛ�A×�AÏ\AÙ�AÙ�AÛ�AÝ�Aß�AÛ�Aß�AÝ�AÙ�Aß�Aã�Aß�AÝ�AÝ�Aß�AÝ�AÙ�AÝ�Aß�Aß�AÛ�AÛ�AÙ�AÛ�AÝ�Aã�A×�AÙ�Aß�AÙ�A×�AÝ�AÓuAÙ�AÓuAÙ�AÓuAÑhAÓuAÕ�AÕ�AËDAÕ�AÛ�AÛ�AÑhAÓuAÍPAËDAÕ�AÇ+AÕ�AÕ�A�hsAÓuAÑhAÏ\AÑhAÕ�AÕ�AÑhAÓuAÓuAÙ�AÓuAÓuAÓuAÓuAÓuAÓuAÏ\AÃAÓuAÓuAÛ�AÇ+AÍPAËDAÓuA×�AÕ�AÛ�AÛ�AÛ�AÙ�AÙ�AÛ�AÙ�AÙ�Aß�AÝ�Aß�Aã�AÝ�Aß�Aã�Aá�Aç�AîAá�AÝ�Aå�Aç�Aå�Aá�Aß�Aá�Aá�Aß�AÙ�AÝ�AÛ�Aã�Aç�Aá�AÓuA×�Aá�Aå�AÙ�Aß�Aá�AÛ�A×�AÑhA×�A��^AÛ�Aá�Aá�Aá�Aá�AÏ\Aá�Aã�Aå�Aå�Aå�Aã�Aá�Aß�Aá�Aã�Aá�AÝ�Aã�Aç�Aé�Aç�Aã�Aã�AìAß�AìAé�Aé�A×�Aã�AîAé�Aé�Aç�Aã�AîAìAé�Aé�AìAå�AîAð!Aé�Aå�Að!Aò-AîAìAîAå�Aò-AîAé�AÓuAã�Aã�Aô9Að!Aé�AöFAöFAöFAöFAöFAöFAò-Aá�Aç�A�v�AìAò-AöFAô9AøRAøRAöFAú^Aú^AöFAú^Aú^AøRAöFAöFAöFAöFAò-Aò-Að!Aò-Aô9AöFAöFAøRAöFAøRAú^Aú^Aú^AøRAú^Aú^AøRAøRAú^AøRAú^AþwAüjAüjAþwAüjAüjAú^AüjAú^Aú^Aú^AøRAøRAøRAú^Aú^AöFAøRAú^AöFAøRAöFAøRAøRAøRAú^Aú^Aú^Aú^Aò-AøRAú^AöFAüjAú^AþwAþwAþwAüjAüjAþwAþwAüjAú^AøRAüjAú^AþwA�ĜA�A�ȴA�ĜA���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�A�ĜA�A�A�A�A�A�ĜA�ĜA�A�ĜA�ƨA�ȴA�ƨA�ĜA�ƨA�A�A�A�A�A���A���A���A�A�A�A�A���A�A���AþwA���A���A�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�A���A�A�A�ĜA�A�ĜA�ƨA�ƨA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ȴA���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AöFAú^AöFAöFAú^AþwA���A���AöFAüjAð!Að!AîAé�Aß�Aß�Aß�Aã�Aã�AÛ�AÓuAÕ�AÑhAÃA�z�A�t�A�x�A�r�A�jA�bNA�^5A�\)A�\)A�\)A�\)A�ZA�XA�XA�ZA�XA�S�A�S�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�@� �@�(�@�1'@�1'@�1'@�9X@�(�@� �@�  @��@� �@� �@� �@�(�@� �@�b@� �@��@��@�1'@�1'@� �@� �@��@��@��@�1@�1@�1@�b@��@�b@�b@��@� �@�1'@�9X@�I�@�I�@�Q�@�bN@�Z@�bN@�bN@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@��@�z�@�r�@�z�@�r�@�j@�bN@�I�@�9X@� �@�b@�b@��m@��m@�ƨ@���@��w@��@�K�@�;d@��@��H@��H@��H@���@��\@�n�@�ff@�^5@�ff@�ff@�ff@�n�@�ff@�ff@�ff@�n�@�n�@�n�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�v�@�n�@�^5@�V@�E�@�5?@�{@���@��@��T@��@��T@��#@���@���@���@���@�r�Aú^AþwAüjAüjAþwAüjAüjAú^AüjAú^Aú^Aú^AøRAøRAøRAú^Aú^AöFAøRAú^AöFAøRAöFAøRAøRAøRAú^Aú^Aú^Aú^Aò-AøRAú^AöFAüjAú^AþwAþwAþwAüjAüjAþwAþwAüjAú^AøRAüjAú^AþwA�ĜA�A�ȴA�ĜA���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�A�ĜA�A�A�A�A�A�ĜA�ĜA�A�ĜA�ƨA�ȴA�ƨA�ĜA�ƨA�A�A�A�A�A���A���A���A�A�A�A�A���A�A���AþwA���A���A�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�A���A�A�A�ĜA�A�ĜA�ƨA�ƨA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ȴA���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AöFAú^AöFAöFAú^AþwA���A���AöFAüjAð!Að!AîAé�Aß�Aß�Aß�Aã�Aã�AÛ�AÓuAÕ�AÑhAÃA�z�A�t�A�x�A�r�A�jA�bNA�^5A�\)A�\)A�\)A�\)A�ZA�XA�XA�ZA�XA�S�A�S�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�@� �@�(�@�1'@�1'@�1'@�9X@�(�@� �@�  @��@� �@� �@� �@�(�@� �@�b@� �@��@��@�1'@�1'@� �@� �@��@��@��@�1@�1@�1@�b@��@�b@�b@��@� �@�1'@�9X@�I�@�I�@�Q�@�bN@�Z@�bN@�bN@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@��@�z�@�r�@�z�@�r�@�j@�bN@�I�@�9X@� �@�b@�b@��m@��m@�ƨ@���@��w@��@�K�@�;d@��@��H@��H@��H@���@��\@�n�@�ff@�^5@�ff@�ff@�ff@�n�@�ff@�ff@�ff@�n�@�n�@�n�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�v�@�n�@�^5@�V@�E�@�5?@�{@���@��@��T@��@��T@��#@���@���@���@���@�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;oG�O�G�O�;o;o;o;o;o;oG�O�;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�p?�=���=��.=�-�=�	B>8�=�F?��K@���=�*�>���@�v�?�j�?x	-@���=��V?If?�EN@�|F> D(@#�?�̣=�
=>�@��=�:>ۓ�@�w�=�`>��@�|�@��{=��=�#�>3�@�x>+�G?�L=s�=��[=�AJ>~�?��d=�
R>��@�{5@���>Ҟ?�ZG@�~|>�4?�C�=D�=|^=��`=�=���=���>(>B??X�?'>PJ�=�D�=ؽf=��=�/>�?>@�r?�WT=OV�=N��=y	l=��K=�ŗ>��@��M?�W�>9rG@�b�?�?�2#@���@��=�Nf>�=�@�s�@Xϖ=�>�=��s>�>;i�@�vK@QXd=�&-=��>Π�>`�@��@���@��#=�X>UG@�t?@�Q=�Z�>%�@��1=��=뤔>�}�@���=��.?-�=��3?�x�@���@��f>�_1>�4?H�@�~�>��q@�{5@��=�4�>#,�>��@��I@�v�@�y>=׼>O"@�$>8ʂ>7`�@�C-=�T�>60�>�o=���@���@��?>�>@S�@���@���>�C�@���@/��>Cc�@���@��a@���>�@���>�<>>��@��X@dR@��"@��"@n�R@��?@���>0�)@��@���@���@���@��X@���@�#d?;(@���@���@��@��.@*��@���@���@��a@��@��q@��q@���@���@��q@��a@���@��"@��"@���@��G@�� @���@��P@���@���@��P@��r@���@���@��z@���@��z@��@��@���@���@��a@���@���@���@���@��@��q@���@��"@���@���@���@���@��P@��a@���@��?@���@���@��r@���@���@��z@���@��z@��z@���@���@���@���@��P@���@���@���@���@���@��z@��z@��z@���@���@���@��a@��a@���@���@��a@���@���@��z@���@��z@��i@���@��a@��@��q@���@��?@��a@���@���@��?@���@���@��H@�� @��i@���@���@���@��/@��r@��@��/@��/@��/@���@���@��Y@��H@���@��@���@���@��a@��@��@���@D�+@���@���@���@���@��H@��H@��"@��@��/@���@���@���@��f@��@��f@��@��Y@��@��"@���@���@��v@��f@��U@��@���@��r@��/@���@���@���@�� @���@���@���@���@��P@���@���@���@��P@���@��"@���@���@��"@���@��G@��@���@���@���@���@���@���@��@���@��@��G@���@���@���@���@���@���@���@���@��a@���@��r@��H@���@��#@��@���@���@��q@���@��@��a@��@���@���@���@��"@���@��"@��"@���@��a@��a@���@���@��@��O@��*@��@���@��h@���@��y@���@��q@���@���@��*@��*@���@���@���@���@��y@���@���@��@��6@��`@��`@���@���@��[@���@���@��`@��`@��@��6@���@��@��1@��y@��@��l@���@��)@��)@��l@���@���@��@���@��@��X@���@���@��@��9@���@���@���@���@���@��9@��9@���@��9@��x@���@��W@���@���@���@��@��@���@���@���@���@���@���@���@���@���@��5@���@���@���@���@��J@��J@��J@���@��[@���@��J@��@��@���@���@��J@���@��J@���@��5@���@��_@���@���@��N@���@���@���@��_@��_@��J@���@��[@��[@���@��_@��@���@��|@���@���@��9@���@��|@��|@��9@��9@��9@��9@��9@���@���@��9@��Z@��@��'@��{@��{@���@���@��8@���@���@���@��^@���@��I@��I@���@��@��I@���@���@���@��I@���@��I@���@��^@���@��I@���@���@���@���@���@���@���@��<@���@���@���@���@��^@���@��n@���@��
@���@���@��<@���@���@���@���@���@���@���@���@���@���@��<@���@��<@���@���@���@��^@��s@��s@��s@���@��^@��@��@���@��@��
@���@���@��b@��^@��@��@���@��@���@���@��@��@���@���@��@��@���@���@���@��<@��<@���@��L@���@��@��a@��@��@���@���@���@��~@��@���@��/@��;@���@��;@��m@���@��@���@��2@���@���@���@��@���@��@���@��!@��B@��B@���@��l@��l@��)@��)@��}@��)@��)@��)@���@��B@��1@���@��u@��@��@���@��:@��)@��@��@��2@���@��"@��?@��D@���@���@��a@���@���@���@��U@���@���@��@���@��U@��@��@��D@���@���@���@���@���@���@��D@���@���@���@���@��@T:�@T;%@T:�@T;%@T;%@T9�@T77@T5i@T6@T9�@T8�@T9�@T:~@T9�@T:*@T9.@T9�@T9.@T;%@T=�@T<K@T<�@T;%@T:�@T:�@T;O@T;�@T;�@T<�@T?h@T>�@T?@TA@TB�@TC�@TG�@TI�@TJb@TL@TN'@TN{@TN�@TOv@TRi@TR�@TS;@TS;@TRi@TR�@TR@TR?@TR@TQD@TP@TOv@TOv@TM+@TK�@TH�@TB�@T@�@T=�@T9X@T6�@T1'@T.�@T+@T'(@T!l@T�@T�@T�@Tx@T�@T �@S��@S�j@S�@S�@S�E@S�E@S��@S�@S�A@S��@S��@S�A@S�@S�g@S��@S�@S�^@S��@S�U@S��@S�U@S�+@S�U@S�U@S�@S�@S�
@S�g@S��@S��@S��@S��@S�&@S��@S��@S�@S��@S�M@S�'@S��@S��@S�@S��@S�0@S�0@��@��@��@��@��@��@��@�X@�C@�X@�C@�H@�q@�q@��@�q@�H@��@��@�3@��@��@��@��@��@��@��@��@��@��@�@��@�&@�@�@�:@�@��@��@��@��@�O@�@��@��@�?@�@��@��@�#:@�!W@�$@�!l@�$�@�$�@�$_@�#�@�$�@�%@�%@�$�@�%1@�%1@�%�@�%�@�%�@�%�@�&@�%�@�%@�%@�%�@�%�@�%�@�%�@�%1@�"}@�">@�"h@�"h@�!�@�"h@�"�@�"�@�#%@�#:@�"�@�#:@�%@�%�@�$_@�$t@�$�@�"�@�"�@�"�@�#�@�"S@�")@�!�@�!�@�"�@�"�@�"�@�#%@�!�@�"@�!�@�"@�!�@�"�@�$5@�$ @�$�@�$t@�%�@�%p@�%�@�%p@�%�@�%F@�$t@�"h@�#�@�#�@�$_@�$t@�$�@�%p@�%�@�$�@�$�@�%@�%1@�%F@�%�@�%p@�&@�%�@�&-@�%�@�%�@�%�@�'�@�(N@�(x@�&�@�&�@�'R@�'�@�'g@�+�@�,R@�+V@�*�@�)�@�*�@�+�@�/0@�+�@�)t@�*@�-w@�,�@�*Z@�/�@�.4@�.@�/E@�.^@�/0@�/�@�0@@�3�@�33@�4�@�4�@�5i@�5@�5~@�6@�6P@�6&@�5T@�5T@�4�@�4�@�4�@�3�@�1�@�1f@�1{@�2M@�28@�0�@�/o@�/�@�/o@�,�@�+�@�*0@�*E@�)J@�(x@�'@�%�@�%�@�%�@�%�@�%�@�%p@�%[@�%@�%1@�%@�$�@�#�@�$@�#�@�$@�$@�$@�#�@�#�@�#�@�#�@�#�@Rc�@Rdo@Rh
@Rg�@Rg�@Rh�@Rh
@Re�@R^_@R_�@Rd�@Re@Re�@Re�@Rb�@RbN@Rd@Rc�@Rc�@Rg�@Rk�@Rg�@Rh
@Rg�@Re�@Rf�@Rd�@Rd�@Rc�@Rg�@Ri�@Ri@Rg�@Ri/@RmH@Ro�@Rp;@Rt�@Ruy@Rv!@R{�@Ry�@Rz@Rz�@R��@R�@R�s@R��@R�I@R��@R��@R��@R�@R��@R�@R�@R�@R$@R~R@R|�@Rxl@Rv�@Rp�@Rl�@Rk{@Rf@Rd�@R^5@RZ�@RZ�@RT@RHA@RD(@R@@R3�@R3r@R1�@R(�@R$�@RO@R�@R@R�@R @R*@Ry@R&@R�@R�@R!@R�@R @R �@R"�@R#�@R#d@R#�@R$@R$�@R%[@R$5@R$@R#�@R!�@R!@R~@R�@R�@R0@R@R	�@R	W@R	-@R�@R�@R�@R�@R@R@R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            444444444344344344434444434434433444344444444433443444444444444444443444444434434433443344443344444334433443444344443344434334443334444434444334333434433343443333333433333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�v�G�O�G�O�@���G�O�G�O�G�O�@�|BG�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@�w�G�O�G�O�@�|�@��|G�O�G�O�G�O�@�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�{8@���G�O�G�O�@�~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��PG�O�G�O�@�b�G�O�G�O�@���@��G�O�G�O�@�s�@XϚG�O�G�O�G�O�G�O�@�vQ@QXcG�O�G�O�G�O�G�O�G�O�@���@��&G�O�G�O�@�tB@�PG�O�G�O�@��.G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���@��gG�O�G�O�G�O�@�~�G�O�@�{4@��G�O�G�O�G�O�@��J@�v�@�y=G�O�G�O�G�O�G�O�G�O�@�C.G�O�G�O�G�O�G�O�@���@��@G�O�@S�@���@���G�O�@���G�O�G�O�@���@��b@���G�O�@���G�O�G�O�@��V@dR@��#@��@n�S@��B@���G�O�@��@���@���@���@��V@���@�#jG�O�@���@���@��@��/G�O�@���@���@��`@��@��r@��q@���@���@��o@��^@���@��#@��"@���@��H@���@���@��R@���@���@��L@��u@���@���@��z@���@��z@��@��@���@���@��_@���@���@���@���@��
@��q@���@��"@���@���@���@���@��O@��`@���@��<@���@���@��o@���@���@��z@���@��z@��z@���@���@���@���@��R@���@���@���@���@���@��}@��z@��z@���@���@���@��b@��`@���@���@��c@���@���@��~@���@��~@��j@���@��b@��	@��r@���@��@@��a@���@���@��B@���@���@��I@��@��i@���@���@���@��.@��v@��@��.@��4@��4@���@���@��^@��L@���@��@���@���@��f@��@��@���G�O�@���@���@���@���@��K@��K@��%@��"@��.@���@���@���@��f@��@��g@��@��Z@��@��&@���@���@��v@��g@��V@��@���@��o@��3@���@���@���@��@���@���@���@���@��O@���@���@���@��R@���@��$@���@���@��$@���@��J@��@���@���@���@���@���@���@��"@���@��@��H@���@���@���@���@���@���@���@���@��_@���@��r@��H@���@��"@��@���@���@��q@���@��@��c@��
@���@���@���@��!@���@�� @��!@���@��f@��Z@���@���@��	@��N@��.@��@���@��i@���@��z@���@��r@���@���@��)@��)@���@���@���@���@��z@���@���@��@��7@��b@��`@���@���@��[@���@���@��a@��f@��
@��6@���@��@��/@��~@��@��n@���@��(@��(@��h@���@���@��@���@��@��U@���@���@��@��:@���@���@���@���@���@��:@��:@���@��:@��u@���@��Z@���@���@���@��@��@���@���@���@���@���@���@���@���@���@��6@���@���@���@���@��N@��N@��K@��@��@��@��@��@��@��@�X@�A@�V@�F@�F@�o@�o@��@�s@�E@��@��@�2@��@��@��@��@��@��@��@��@��@��@�@��@�&@�@�@�:@�@��@��@��@��@�M@�
@��@��@�B@�
@��@��@�#;@�!V@�$
@�!o@�$�@�$�@�$^@�#�@�$�@�%	@�%@�$�@�%2@�%.@�%�@�%�@�%�@�%�@�&@�%�@�%	@�%@�%�@�%�@�%�@�%�@�%.@�"~@�">@�"k@�"g@�!�@�"g@�"�@�"�@�#%@�#:@�"�@�#8@�%@�%�@�$`@�$r@�$�@�"�@�"�@�"�@�#�@�"R@�"&@�!�@�!�@�"�@�"�@�"�@�#%@�!�@�"@�!�@�"@�!�@�"�@�$6@�$!@�$�@�$t@�%�@�%p@�%�@�%m@�%�@�%D@�$r@�"e@�#�@�#�@�$]@�$w@�$�@�%m@�%�@�$�@�$�@�%@�%2@�%G@�%�@�%k@�&@�%�@�&-@�%�@�%�@�%�@�'�@�(P@�(y@�&�@�&�@�'R@�'�@�'f@�+�@�,N@�+V@�*�@�)�@�*�@�+�@�//@�+�@�)s@�*@�-t@�,�@�*[@�/�@�.2@�.@�/D@�._@�/2@�/�@�0B@�3�@�31@�4�@�4�@�5i@�4�@�5|@�6@�6P@�6&@�5S@�5U@�4�@�4�@�4�@�3�@�1�@�1h@�1@�2L@�26@�0�@�/n@�/�@�/j@�,�@�+�@�*.@�*F@�)K@�(w@�'@�%�@�%�@�%�@�%�@�%�@�%u@�%]@�%@�%4@�%	@�$�@�#�@�$@�#�@�$@�$@�$@�#�@�#�@�#�@�#�@�#�@Rc�@Rds@Rh@Rg�@Rg�@Rh�@Rh@Re�@R^^@R_�@Rd�@Re@Re�@Re�@Rb�@RbP@Rd@Rc�@Rc�@Rg�@Rk�@Rg�@Rh
@Rg�@Re�@Rf�@Rd�@Rd�@Rc�@Rg�@Ri�@Ri @Rg�@Ri2@RmK@Ro�@Rp=@Rt�@Ru{@Rv"@R{�@Ry�@Rz@Rz�@R��@R�"@R�v@R��@R�K@R��@R��@R��@R�@R��@R�@R�@R�@R&@R~N@R|�@Rxk@Rv�@Rp�@Rl�@Rk{@Rf@Rd�@R^8@RZ�@RZ�@RT
@RHB@RD&@R@@R3�@R3r@R1�@R(�@R$�@RP@R�@R@R�@R @R+@Rz@R%@R�@R�@R @R�@R @R �@R"�@R#�@R#`@R#�@R$@R$�@R%[@R$5@R$@R#�@R!�@R#@R}@R�@R�@R-@R@R	�@R	Z@R	+@R�@R�@R�@R�@R@R@R�@��@��@��@��@��@��@��@�X@�A@�V@�F@�F@�o@�o@��@�s@�E@��@��@�2@��@��@��@��@��@��@��@��@��@��@�@��@�&@�@�@�:@�@��@��@��@��@�M@�
@��@��@�B@�
@��@��@�#;@�!V@�$
@�!o@�$�@�$�@�$^@�#�@�$�@�%	@�%@�$�@�%2@�%.@�%�@�%�@�%�@�%�@�&@�%�@�%	@�%@�%�@�%�@�%�@�%�@�%.@�"~@�">@�"k@�"g@�!�@�"g@�"�@�"�@�#%@�#:@�"�@�#8@�%@�%�@�$`@�$r@�$�@�"�@�"�@�"�@�#�@�"R@�"&@�!�@�!�@�"�@�"�@�"�@�#%@�!�@�"@�!�@�"@�!�@�"�@�$6@�$!@�$�@�$t@�%�@�%p@�%�@�%m@�%�@�%D@�$r@�"e@�#�@�#�@�$]@�$w@�$�@�%m@�%�@�$�@�$�@�%@�%2@�%G@�%�@�%k@�&@�%�@�&-@�%�@�%�@�%�@�'�@�(P@�(y@�&�@�&�@�'R@�'�@�'f@�+�@�,N@�+V@�*�@�)�@�*�@�+�@�//@�+�@�)s@�*@�-t@�,�@�*[@�/�@�.2@�.@�/D@�._@�/2@�/�@�0B@�3�@�31@�4�@�4�@�5i@�4�@�5|@�6@�6P@�6&@�5S@�5U@�4�@�4�@�4�@�3�@�1�@�1h@�1@�2L@�26@�0�@�/n@�/�@�/j@�,�@�+�@�*.@�*F@�)K@�(w@�'@�%�@�%�@�%�@�%�@�%�@�%u@�%]@�%@�%4@�%	@�$�@�#�@�$@�#�@�$@�$@�$@�#�@�#�@�#�@�#�@�#�@Rc�@Rds@Rh@Rg�@Rg�@Rh�@Rh@Re�@R^^@R_�@Rd�@Re@Re�@Re�@Rb�@RbP@Rd@Rc�@Rc�@Rg�@Rk�@Rg�@Rh
@Rg�@Re�@Rf�@Rd�@Rd�@Rc�@Rg�@Ri�@Ri @Rg�@Ri2@RmK@Ro�@Rp=@Rt�@Ru{@Rv"@R{�@Ry�@Rz@Rz�@R��@R�"@R�v@R��@R�K@R��@R��@R��@R�@R��@R�@R�@R�@R&@R~N@R|�@Rxk@Rv�@Rp�@Rl�@Rk{@Rf@Rd�@R^8@RZ�@RZ�@RT
@RHB@RD&@R@@R3�@R3r@R1�@R(�@R$�@RP@R�@R@R�@R @R+@Rz@R%@R�@R�@R @R�@R @R �@R"�@R#�@R#`@R#�@R$@R$�@R%[@R$5@R$@R#�@R!�@R#@R}@R�@R�@R-@R@R	�@R	Z@R	+@R�@R�@R�@R�@R@R@R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            444444444344344344434444434434433444344444444433443444444444444444443444444434434433443344443344444334433443444344443344434334443334444434444334333434433343443333333433333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9$O9$�9%89%r9%�9%o9%r9$�9$�9$�9$�9#�9$$9$$9$P9$(9#�9#�9#�9#�9$v9#�9#�9$�9$:9$�9%79%J9%^9%[9"�9#L9&�9$�9'�9&�9'9'	9'9'19'X9&�9'}9'i9'9%�9'}9'19'9+b9)�9,"9)�9,�9,�9,p9+�9,�9-9-"9,�9-59-29-�9-�9-�9-�9.9-�9-9-9-�9-�9-�9-�9-29*�9*w9*�9*�9*+9*�9+9*�9+N9+a9+%9+_9-9-�9,r9,�9,�9*�9*�9*�9+�9*�9*a9*+9*(9*�9+*9+(9+N9*&9*N9)�9*N9*(9*�9,K9,89,�9,�9-�9-o9-�9-l9-�9-F9,�9*�9+�9+�9,o9,�9,�9-l9-�9,�9,�9-9-59-I9-�9-j9-�9-�9.9-�9-�9-�9/f9090A9.�9.�9//9/~9/A93^93�92�92)91�92%9396|93p91)91�94�9492 96�95�95~96�95�9696�97|9:�9:59;�9;�9<E9;�9<V9<�9=9<�9<09<29;�9;�9;�9:�98�98�98�99a99L9896�97*96�94!9391�91�9190?9.�9-�9-�9-�9-�9-�9-t9-]9-#9-79-9,�9+�9,%9, 9,'9,&9,(9+�9+�9+�9+�9+�8�j(8�j�8�m�8�m�8�m�8�n�8�m�8�k�8�d�8�f[8�k8�k<8�k�8�k�8�i@8�h�8�jN8�i�8�j&8�m�8�qM8�m8�m�8�m�8�k�8�l�8�j�8�j�8�j8�m�8�oT8�n�8�m�8�o8�r�8�t�8�u�8�y�8�zr8�{8À8�~<8�~�8�v8Ä�8ÄV8Ä�8Ä�8Ä}8Ä�8Ä�8Ä�8ÅA8Æ8ÄR8Ä.8Æ)8Ãl8Â�8ÁH8�},8�{�8�v8�r�8�q(8�l%8�k8�d�8�a�8�a�8�[b8�Pq8�L�8�H�8�=D8�=8�;r8�38�/�8�){8�(8�'X8�'�8�(C8�(k8�)�8�)S8�)8�(�8�*<8�+8�+(8�+�8�-r8�.�8�./8�.�8�.�8�/D8�08�.�8�.�8�.[8�,�8�*?8�(�8�%�8�"z8�58�}8�(8�8��8�e8�z8��8��8��8��8��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BPBVBVBVBVBVBVBVBPB\B�B&�B5?B8RB;dBA�BN�BXB[#BbNBcTBbNBaHBe`Br�B|�B�B��B�B�FB�qB��BB��B�wB�wB�qB��B	7B)�B/B-B=qBB�B9XB49B�B'�B)�B,B1'B+B#�B(�B.B33B33B33B33B2-B1'B/B+B'�B+B,B(�B�B�BPBB�B�ZB��B�}B�B��B��B�DBq�B]/BQ�BE�B0!B{B  B��B�sB�;B�
B��B��B�-B��B�oB�7By�Bk�BW
B<jB�B�B�BhBPB1BB
��B
�ZB
��B
ĜB
��B
�jB
�FB
�3B
�'B
�B
��B
��B
�+B
q�B
iyB
W
B
L�B
@�B
8RB
%�B
hB
JB

=B
B	��B	�B	�ZB	�B	ǮB	��B	�bB	�B	s�B	l�B	dZB	^5B	T�B	L�B	D�B	<jB	9XB	49B	-B	$�B	�B		7B	+B	B	B	B	B	1B	PB	+B��B��B��B��B�B�B�B�B�mB�HB�)B��B��B�wB�B��B��B��B�uB�PB�DB�DB�=B�7B�7B�1B�1B�+B�1B�%B�B�B�B�B~�B~�B}�B{�Bz�B{�B|�B}�B� B|�Bz�B|�B}�B|�By�By�Bw�Bu�Bt�Bq�Bq�Bp�Bn�Bm�Bn�Bm�Bm�Bm�Bp�Bp�Bu�Bv�Bw�Bx�Bx�By�By�Bz�Bz�By�Bz�B{�B}�B�B�B�B�B�%B�%B�B�B�B�B�B�B�B�B�%B�+B�+B�1B�VB�VB�VB�VB�\B�\B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�-B�9B�FB�dB�jB�jB�dB�FB�9B�3B�9B�-B�!B�B��B��B��B�3B�FB�RB�dB��BÖBB��B�wB�wB��BȴB��B��B�
B�B�)B�/B�/B�HB�HB�NB�`B�fB�B�B�B��B��B��B��B	B	B	B	B	%B	+B	1B	
=B	PB	hB	�B	�B	 �B	.B	7LB	@�B	C�B	E�B	F�B	I�B	L�B	Q�B	YB	]/B	\)B	^5B	`BB	bNB	bNB	cTB	ffB	ffB	gmB	gmB	hsB	m�B	p�B	o�B	p�B	r�B	s�B	t�B	u�B	y�B	{�B	}�B	�B	�B	�B	�+B	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�?B	�XB	�^B	�jB	�wB	��B&�B	B	ĜB	ĜB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�BB	�NB	�NB	�TB	�NB	�NB	�NB	�HB	�BB	�;B	�BB	�BB	�BB	�;B	�/B	�#B	�#B	�/B	�/B	�/B	�/B	�;B	�NB	�TB	�NB	�NB	�NB	�NB	�TB	�TB	�NB	�NB	�HB	�TB	�`B	�`B	�mB	�yB	�B	�sB	�ZB	�ZB	�ZB	�yB	��B	�zB
{B
�B
�B
 BB
'RB
1�B
8�B
?�B
G�B
M�B
RB
U�B
[=B
_�B
d�B
i�B
oiB
sMB
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��@L�b>��>�$5?�>�2?ov?�AATB�Y?D@7aBjlA�"@��Bd�?Gk@T%J@�?Bv?';�A}�A�u?,�?9�B��?�_@CB�?�6@��Bh�B��>倀?�?h�Bn�?i��@b�>���>�9�>�LQ?/�t@��P>�*�?*��Bd%Bn�?(yA�B%�?M�<A$>~O3>�"�>��?Jw?�o>�KE?]}�@�̩@C`?��>�i�?Ua?�v?��?���BiYA$��>�1�>�K�>���>�? 5?I��B�A�?q7�BR�A8�f@�L^B��B��?�@��BjA��9>ē�>��?O� ?shIBk�A���>�5a?��@��?9�VA`Q�BkBuJ?P?>Q+BgNBj�>�F�?V�1A��>݁�?Mf?��LB��?
+@;9�?R�A.=Br�B�i@3��?+�@���Br?��Bm	B��>�D�?Wݘ?5�Bp0Bh�BɄ?��??�wAD��?v��?o�B`:>�e?l�t?�M�?$PBz�Bu�?;�lA�%�Br�BX.@*\�A��A��\?|��Bs.BuEB��?EnYB|?F�?w��Bt�A��(BxWBu�A��sBr�B�B?dq�Bs�BtBsBt�Bw&B�(B�P@��#Bt/Bt�BuMB��A�URBs�Bt�BvBuBumBvMBu�Bp�BzXBr�Bu�BuBu�Bu�Bw�BuNBw�BuBu:Bt�BuBvvBtcBv2BvBw�BtuBu�Bu�BubBu:BqkBucBu�Bt�BukBv�Bx�Bt�Bu�BuYBu�Bs�BulBv�BvBwBt�Bu:Bu�Bu�BwvBvPBw�Bu�Bv
Bv
Bu�Bt�BuZBu�BuBu�Bs�Bu�Bw2ByDBv�Bv�Bv
Bu�Bt�Bv�BuEBvBvBt�Bs�Bt�BuZBu3BulBuDBu�BtyBuEBu�Bw�Bt:Bu�Bv�BuZBr�BvxBv(Bv}Bs�BvFBt;Bw�BtlBv�Bt�BvmBv�BwBvXBvXBy(BvBt
BsBv�BvBx=ByBt�By�BuXBvA���Bv�BwPBw3Bu�BuoBuwBv
Bv'BwBtlBsBsBt�BuBt�Bt2Bt;Bz�Bu;Bu�Bn�Bz[Bv�Bv�BuBu2Bu�Bs�Bs�BsYBu(BuBs�Bu1BvcBs�BuBt�BrXBu�BtNBs�Bt;Bs1BpBt;Bv;Bs�Bs�BtBtBt�Bs�Bs�Br�Bs�Br�Bu�Bt�BrBq�BynBxBt�Br'BvcBu�BteBv)Bt�BwBtNBU�Bu�Bt�Bt�BuvBs�B{vBtvBtMBsBs'BsFBslBt�Bu
BtCBr�BtBvBs�BsiBq�Bu�Bv<BuMBs�BxBt�BuBvaBy�Bu�Bs�Bs�Bs�BuXBv�Bs�BtkBuBubBs�BvNBt/Bs~Bu�Bw�Bs�Bs�Bs�BtcBtNBw�BsMBt&BtaBx�By%Bw�Bs�BukBx&Bs�Bs�Br�BsDBsMBr�Bs�Bw�Bs�B�SBu�Bv.Bt�Bu�Bt8Bt8Bu`Bs�Bs Bt�BsFBr�Bs�BtBr�BrLBq�Bs�Bt Bt�BuRBtzBs�Bs�BsBt~Bs�Bs&Bs�BsdBs�BsBr�Bs�Bt$BsMBtBt�Bt3Br�BsBs�Br�Bs�Bs�Br�BtBs�Br�Bt>BsBtTBs�Bs�Bs2BspBt�Bs_Bt�Bs�Bs�Bt Bs�Bs�BsCBs�Bs�Bu�Bt�BuABtiBt�Bs�Bs�Bs�BuTBuKBuCBtcBtZBt�Bt�BuBv8BttBw	Bx�BtBt?BuTBt�BtkBt�BtZBu�BtIBt�Bt�Bt�BuBteBt�Bs�Bt�BtCBs�Bt)Bt�Bt,Bs�Bt�BuNBs(Bt�Bt�Bu�Bu9Bu�BuBtyBtpBt�Bt�BvBt�Bt�BrBu�Bt�Bt�Bt�Bt�BvBt�Bt�Bs�Bt�Bt�BudBt�Bt�Bt,Bt�Bt�Bu1Bu(Bv;Bu*Bu5Bu-Bu$BujBt9Bu�Bu�BuQBu�Bs�Bt8BuBuuBvYBudBt�Bu�BuJBt�Bt�Bu9BtbBu�BuBt�Bt�Bu�Bt�Bu�BuOBvBu�Bv5Bv�Bu�Bv/Bu�Bu�BvdBx\Bv�BxBv�BvBv�Bx�Bw�Bv�Bv�Bw8Bw�BxMBx�BxQBxHBx@Bw�By/B{B~OB�B��B�B�LB��B��B�B�*B��B��B��B��B��B�B��B�AB�%B��B��B��B�AB�iB��B�B�QB��B�|B��B�gB��B�~B��B�\B��B�B��B�B�4B�^B�B�jB�B�B��B��B��B��B��B��B�gB�AB�VB�bB�YB�QB��B;�B:�B:LB;�B:aB9RB:�B8B<�B=KB:UB;�B;_B:�B<B:5B;�B; B=�B;:B:�B<AB<%B=�B<�B=!B>VB><B?�B?�B>GB@oB@�B@�BA�BABA�BA�BA�B@B@?B@oB@�BC�B>�B@$B>�B?cB?gB=�B?B=�B=B=3B<�B;B:�B:�B8iB6%B:B7�B7�B6�B3�B88B5|B4�B3�B3TB2�B6PB6�B0�B4�B8qB3B9B8?B9�B:�B;nB;%B;)B:�B:jB:�B;B;�B:�B:?B:�B;iB:�B:lB:�B9oB:�B9fB9iB9 B8�B7JB8|B8"B6�B6�B6B7+B8-B9�B9rB8�B8�B8�B8�B:B9�B:hB9;B1B�B6BiB�BXBGB�B�B|B`BlBOBFBeBiB0BLB�B�BB|B8B;B�B*B�BBBB�B�B7B�B2B2BLB�B�B�B�BhBB�B,B�B�B1B|BYBZBkB�BB�B�B�B;BFBRB"BLBCB�B�B�B�B%B_BB�B�B�B�B8B�B�B$BvB:B�B!B�B^B�B�B�B�B�B�B
B�B\B�B�B�B�B}BB�B�B�B�B�B�B�B�BPBwB�B6B�B�B�BKB^B�BaBB)B�B�B�B�B�B�B�B(B�B�B�B�BNBdBwBB�BBB&BYB�B�B�B�BaBB�B�B BfB#B�BBBRByBABkB�B�B�B�B8B8BBB�B�B�B�BVBiB�BUB�B!B4B!�B!B�B�B8BB!;B�B"�B"�B#�B$AB&bB&B&!B%DB%'B'B(�B(�B)�B,�B.�B/�B.9B/�B2B3�B4fB5B4�B4�B4�B5`B6B5�B5B5�B6�B6B7B6�B7�B6�B7�B7vB7ZB7�B7�B7�B	�8B	�jB	��B	�xB	�B	�B	�B	��B	��B	�B	�B	�/B	�qB	�B	�`B	��B	�B	��B	��B	�{B	�B	�5B	�vB	�ZB	�B	�{B	��B	�B	�B	��B	��B	�B	�B	�}B	�]B	��B	�-B	�.B	�B	�
B	��B	�UB	�B	�B	��B	�B	�B	��B	�mB	�B	�B	�B	�B	�&B	��B	�B	� B	��B	�XB	�:B	�"B	��B	�B	��B	��B	�B	�"B	�nB	�.B	��B	�@B	�B	�B	�AB	�B	�SB	��B	�B	�	B	�eB	�TB	�B	��B	�1B	�CB	�B	��B	�|B	�CB	�:B	�B	��B	�9B	�OB	�B	��B	�B	�B	�UB	��B	��B	�B	�^B	��B	�B	��B	��B	�B	��B	�VB	�B	�B	�UB	��B	�:B	�B	��B	��B	��B
:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444444344344344434444434434433444344444444433443444444444444444443444444434434433443344443344444334433443444344443344434334443334444434444334333434433343443333333433333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999BYB]BaB_B]B_B_G�O�G�O�G�O�G�O�G�O�G�O�G�O�B;nG�O�G�O�BXB[,BbYBc`BbXBaPG�O�Br�B|�G�O�G�O�B�B�RB�zB��BB��B��B��B�xB��B	DB*B/%B-B=}BB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B_BB�B�dB��B��B�B��B��B�OBq�B]<BQ�BE�B0.B�B B��B�B�GB�B��B��B�9B��B�yB�EBy�Bk�BWB<vB�B�B�BsB\B:BB
��B
�fB
��B
ĩB
��B
�xB
�PB
�=B
�1B
�B
��B
��B
�4B
q�B
i�B
WB
L�B
@�B
8aB
%�B
rB
VB

HB
B	��B	�B	�eB	�"B	ǻB	�	B	�mB	�B	s�B	l�B	dgB	^CB	U	B	L�B	D�B	<vB	9gB	4FB	-B	$�B	�B		CB	7B	.B	 B	+B	+B	?B	]B	7B�B��B��B��B��B��B�B�B�zB�VB�8B�B��B��B�B��B��B��B��B�^B�RB�TB�JB�DB�EB�?B�@B�8B�?B�3B�'B�B�B�BBB~B{�Bz�B{�B|�B~B�B|�Bz�B|�B~B|�By�By�Bw�Bu�Bt�Bq�Bq�Bp�Bn�Bm�Bn�Bm�Bm�Bm�Bp�Bp�Bu�Bv�Bw�Bx�Bx�By�By�Bz�Bz�By�Bz�B{�B~B�B�)B�(B�&B�5B�2B�-B�B�B�B� B�(B�&B�-B�4B�<B�9B�@B�gB�eB�gB�dB�iB�iB�kB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�<B�;B�HB�WB�uB�zB�{B�uB�UB�GB�EB�HB�=B�2B�$B�
B��B� B�AB�VB�aB�pB��BçB B��B��B��B��B��B��B�B�B�.B�:B�<B�?B�XB�WB�]B�pB�vB�B�B��B��B��B��B�	B	B	(B	.B	.B	4B	:B	?B	
IB	`B	yB	�B	�B	 �B	.#B	7]B	@�B	C�B	E�B	F�B	I�B	L�B	Q�B	Y&B	]?B	\;B	^EB	`RB	b`B	b_B	ceB	ftB	ftB	g{B	g|B	h�B	m�B	p�B	o�B	p�B	r�B	s�B	t�B	u�B	y�B	{�B	~B	�B	�/B	�0B	�:B	�XB	�gB	�mB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�1B	�/B	�=B	�NG�O�B	�nB	�zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�,B	�,B	�0B	�9B	�8B	�9B	�>B	�RB	�]B	�[B	�dB	�^B	�^B	�_B	�YB	�QB	�KB	�QB	�SB	�SB	�LB	�AB	�1B	�3B	�>B	�=B	�=B	�?B	�IB	�]B	�cB	�^B	�\B	�\B	�^B	�eB	�dB	�_B	�\B	�VB	�cB	�nB	�oB	�}B	�B	�B	�B	�iB	�gB	�jG�O�B	�B	��B
�B
�B
�B
 SB
'aB
1�B
8�B
?�B
HB
M�B
RB
U�B
[IB
_�B
e	B
i�B
ovB
s`B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�bG�O�G�O�BjvG�O�G�O�Bd�G�O�G�O�G�O�BvG�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B� G�O�G�O�Bh�B��G�O�G�O�G�O�Bn�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bd2Bn�G�O�G�O�B%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BibG�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�BR�G�O�G�O�B��B��G�O�G�O�Bj(A��JG�O�G�O�G�O�G�O�Bk�A���G�O�G�O�G�O�G�O�G�O�BkBuUG�O�G�O�Bg\Bj�G�O�G�O�A��G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�Br�B�tG�O�G�O�G�O�BrG�O�BmB��G�O�G�O�G�O�Bp9Bh�BɍG�O�G�O�G�O�G�O�G�O�B`DG�O�G�O�G�O�G�O�Bz�Bu�G�O�A�%�Br�BX8G�O�A��G�O�G�O�Bs=BuOB��G�O�B|&G�O�G�O�Bt�A��4BxdBu�A�ɄBr�B�OG�O�Bs�Bt'BsBu
Bw0B�1B�]G�O�Bt:BuBuZB��G�O�Bs�Bt�BvBuBuxBvWBu�Bp�Bz`Br�Bu�BuBu�Bu�Bw�BuXBx	BuBuEBt�BuBv�BtlBv;BvBx BtBu�Bu�BuoBuEBqsBuoBu�Bt�BuuBv�Bx�Bt�Bu�BuaBu�Bt BuxBv�BvBwBt�BuEBu�Bu�Bw�Bv^Bw�Bu�BvBvBu�Bt�BufBu�BuBu�Bt Bu�Bw<ByNBv�Bv�BvBu�Bt�Bv�BuOBvBv	Bt�Bs�Bt�BufBuABuuBuOBu�Bt�BuOBu�Bw�BtGBu�Bv�BufBr�Bv�Bv4Bv�Bs�BvQBtEBw�BtvBv�Bt�BvyBv�Bw)BveBveBy2BvBtBs Bv�Bv'BxHByBt�BzBu`BvG�O�Bv�BwZBw<Bu�BuzBu�BvBv4Bw)BtvBs*Bs Bt�Bu(Bt�Bt?BtEBz�BuHBu�Bn�BzfBv�Bv�Bu(Bu?Bu�Bs�Bs�BsbBu2Bu�Bs�Bu;BvmBt Bu&Bt�BrdBu�BtXBtBtEBs9Bp)BtEBvFBs�Bs�Bt.BtBt�Bs�Bs�BsBs�Br�Bu�Bt�Br�Bq�ByzBx(Bt�Br0BvmBu�BtnBv4Bt�Bw)BtXBU�Bu�Bt�Bt�Bu~Bs�B{�Bt�BtXBs+Bs1BsSBstBt�BuBtLBsBt�Bv"Bs�BsrBq�Bu�BvJBuZBs�Bx(Bt�BuBvmBy�Bv
Bs�Bs�Bs�BuaBv�Bs�BttBuBuoBs�BvYBt7Bs�Bu�Bw�Bs�Bs�Bs�BtmBtYBw�BsZBt1BtjBx�By/Bw�Bs�BuuBx3Bs�Bs�Br�BsLBsWBr�BtBw�Bs�B�_Bu�Bv8Bt�Bu�BtFBtFBulBs�Bs
Bt�BsSBr�BtBt%Br�BrXBrBs�Bt	Bt�Bu]Bt�BtBs�Bs&Bt�Bs�Bs1Bs�BsoBs�BsBr�Bs�Bt2BsZBtB<B�BABsB�BaBQB�B�B�BiBtBUBQBpBrB7BWB�BB"B�BABCB�B5BBBBB�B�BBB�B=B;BUB�B�B�B�BoBB�B2B�B�B=B�BcBdBvB�BB�B�B
BCBQB]B)BWBKB�B�B�B�B/BiBB�B�B�B�B>B�B�B-B�BCB�B+B�BfB�B�B�B�B�B�BB�BfB�B�B�BB�BB�B�B�B�B�BB�B BWBB�BAB�B�B�BRBiBBnB#B2B�B�B�B�B�B�B�B2B�BB�B�BWBmB�BB�BNB1BdB�B�B�B�BkB�B�B�B+BsB-B�BB)B_B�BKBsB�B�B�B�BABAB
BB�B�B�B�B`BqB�BaB�B!B?B!�B!(B�B�BABB!CB�B"�B#B#�B$MB&mB&,B&.B%NB%1B'$B(�B(�B)�B,�B.�B/�B.EB/�B2B3�B4rB5!B4�B4�B4�B5mB6B5�B5B5�B7B6 B7B6�B7�B6�B7�B7�B7cB7�B7�B7�B	�GB	�{B	��B	�B	�B	�(B	�B	�B	��B	�B	�-B	�AB	�B	�B	�qB	�	B	�B	��B	��B	�B	�.B	�DB	�B	�hB	��B	�B	��B	��B	�%B	��B	�B	�B	��B	�B	�nB	��B	�=B	�AB	��B	�B	��B	�cB	�B	�#B	�B	�B	��B	��B	�}B	��B	�B	�B	��B	�8B	��B	�B	�1B	��B	�eB	�JB	�2B	��B	�B	�B	��B	�#B	�0B	�B	�=B	�B	�MB	�B	�B	�SB	�B	�cB	��B	�B	�B	�uB	�aB	�B	�B	�@B	�RB	�B	��B	�B	�RB	�JB	��B	��B	�IB	�`B	�B	��B	�B	�B	�dB	��B	� B	��B	�oB	��B	�/B	��B	��B	�.B	��B	�cB	��B	�B	�dB	��B	�IB	�B	��B	��B	��B
KB<B�BABsB�BaBQB�B�B�BiBtBUBQBpBrB7BWB�BB"B�BABCB�B5BBBBB�B�BBB�B=B;BUB�B�B�B�BoBB�B2B�B�B=B�BcBdBvB�BB�B�B
BCBQB]B)BWBKB�B�B�B�B/BiBB�B�B�B�B>B�B�B-B�BCB�B+B�BfB�B�B�B�B�B�BB�BfB�B�B�BB�BB�B�B�B�B�BB�B BWBB�BAB�B�B�BRBiBBnB#B2B�B�B�B�B�B�B�B2B�BB�B�BWBmB�BB�BNB1BdB�B�B�B�BkB�B�B�B+BsB-B�BB)B_B�BKBsB�B�B�B�BABAB
BB�B�B�B�B`BqB�BaB�B!B?B!�B!(B�B�BABB!CB�B"�B#B#�B$MB&mB&,B&.B%NB%1B'$B(�B(�B)�B,�B.�B/�B.EB/�B2B3�B4rB5!B4�B4�B4�B5mB6B5�B5B5�B7B6 B7B6�B7�B6�B7�B7�B7cB7�B7�B7�B	�GB	�{B	��B	�B	�B	�(B	�B	�B	��B	�B	�-B	�AB	�B	�B	�qB	�	B	�B	��B	��B	�B	�.B	�DB	�B	�hB	��B	�B	��B	��B	�%B	��B	�B	�B	��B	�B	�nB	��B	�=B	�AB	��B	�B	��B	�cB	�B	�#B	�B	�B	��B	��B	�}B	��B	�B	�B	��B	�8B	��B	�B	�1B	��B	�eB	�JB	�2B	��B	�B	�B	��B	�#B	�0B	�B	�=B	�B	�MB	�B	�B	�SB	�B	�cB	��B	�B	�B	�uB	�aB	�B	�B	�@B	�RB	�B	��B	�B	�RB	�JB	��B	��B	�IB	�`B	�B	��B	�B	�B	�dB	��B	� B	��B	�oB	��B	�/B	��B	��B	�.B	��B	�cB	��B	�B	�dB	��B	�IB	�B	��B	��B	��B
KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444444344344344434444434434433444344444444433443444444444444444443444444434434433443344443344444334433443444344443344434334443334444434444334333434433343443333333433333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011535302020090115353020200901153530202009011535302020090115353020200901153530202009011535302020090115353020200901153530202009011535302020090115353020200901153530AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202121102018112021211020181120212110    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121102018112021211020181120212110  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121102018112021211020181120212110  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011535302020090115353020200901153530  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                