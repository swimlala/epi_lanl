CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  C   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:15Z creation      
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
resolution        =���   axis      Z        '$  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '$  v    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '$  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� &(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$ /�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '$ W   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ~<   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '$ �   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �,   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '$ ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$ �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� @   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 80   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '$ A�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � i    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   i�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   u�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20190219181715  20200831164924  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�T��*@�T��*@�T��*111 @�T���?>@�T���?>@�T���?>@5�XbM�@5�XbM�@5�XbM��c|Q���c|Q���c|Q��111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`ffBh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�HD� D�VfD���D��3D��D�8 D���D��D��D�G�D�j�Dǯ
D��D�>�D�|{D�)D���D�'
D�\D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;������������L�;L�;����������;�������    �L�;����L��=��ͽ��;������������L�ͽ��;��������L�;��������L�;������������L�ͽ��;��������L�;������������������������L�;����L�;����������ͽ��;��������L�;����������������L�;��������L�;��������L�;L�;������������L�;���        �L�;����L�;��������L�;L�;������;����������ͽ��ͽ��;����L�;����L�;L�;��������L�;L�;L�;����������������L��    �L�;L�;L�;����������������L�;����L�;��������������������������;������������L�;��������L��    �������������L�ͽ��ͽ��;����L�;L�;��������L�ͽ��;��������������ͽ��;��������L�;����L�;�������=���    ���������L�;L�;��������L��        �L�;����L�ͽ���=���=��ͽ��;������;L�ͽ���=���=��;L�;����L�;L�;���    �L�;�����������=���>L��>L�;L�;����L�;L�ͽ���    �L�;����L�;������;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�;�����������    �L��    �L�;L�ͽ��ͽ��ͽ��ͽ��;L�;L��            =���=���    =���=���    =���        =���=���=��ͽ��ͽ���        =���    ���ͽ���=���            ���ͽ���            ���ͽ���=���    ����=���=���    �L��    ����        ����    =��ͽ���        ���ͽ��ͽ���            =���        ����    =���    =���                    ����    ����        =���        ����    ���ͽ��ͽ���    ����    �L��=���        ���ͽ���        =��ͽ��ͽ��ͽ���            =���=��ͽ���        ����=��ͽ���    ����    ����    ����=���    =���>L��        >L��=���=���                    ���ͽ��;L��    ���ͽ���        ����        =���        =���    >L��=���        =���=���=���                    ����        ���ͽ���    ����        =��ͽ���        =���            ����                ���ͽ���                =���=��ͽ��ͽ��ͽ���                ���ͽ���    =���    =���>L��    ����=���                    ���ͽ���=��ͽ���            ���ͽ���                            ����    =���    ���;L��        ����    ���ͽ��ͽ���        =���    =���>L��>L��>���>���>���>���>���?   ?   ?   ?��?��?333?333?333?L��?fff?fff?�  ?�  ?�  ?���?���?���?���?���?�ff?�ff?�ff?�33?�33?�33?�  ?���?���?���?ٙ�?ٙ�?�ff?�33?�33?�33?�33@   @   @ff@ff@ff@��@��@33@33@��@   @   @&ff@&ff@,��@333@9��@@  @@  @L��@S33@S33@`  @`  @fff@l��@s33@y��@y��@�  @�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�  @�ff@���@���@���@�33@�33@�ff@���@���@�  @�33@�ff@ə�@�  @�  @�ff@ٙ�@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A��AffA  A33A33AffA  A��A33A��AffA  A��A33A��AffA!��A#33A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A4��A4��A8  A9��A;33A<��A>ffAA��AC33AD��AFffAI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ai��Al��Ap  Aq��At��AvffAy��A{33A~ffA���A���A�33A�  A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A͙�A�33A�  Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�ffA�  Dp� Dp�fDp��Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr@ DrFfDrL�DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt�3Dt��Du  @,��@333@9��@@  @@  @L��@S33@S33@`  @`  @fff@l��@s33@y��@y��@�  @�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�  @�ff@���@���@���@�33@�33@�ff@���@���@�  @�33@�ff@ə�@�  @�  @�ff@ٙ�@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A��AffA  A33A33AffA  A��A33A��AffA  A��A33A��AffA!��A#33A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A4��A4��A8  A9��A;33A<��A>ffAA��AC33AD��AFffAI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ai��Al��Ap  Aq��At��AvffAy��A{33A~ffA���A���A�33A�  A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A͙�A�33A�  Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�ffA�  Dp� Dp�fDp��Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr@ DrFfDrL�DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt�3Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @@  @�ff@�33A33A#33AC33Ac33A���A���A���A���A���Aљ�A�fgA�B ��B��B��B��B ��B(��B133B8��B@��BH��BP��BX��Ba33Bh��Bp��BxfgB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqgDq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�3Dy�D�fD�\�D��GD�ٙD�GD�>fD��RD��zD�)D�ND�qGDǵpD��D�EDڂ�DಏD��D�-pD��D�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=��ͽ��ν��ν��αL�ͱL�ͽ��ν���=��ͽ��ν���>L�ͱL�ͽ��αL��>���=��ͽ��ν��ν��αL��=��ͽ��ν��αL�ͽ��ν��αL�ͽ��ν��ν��αL��=��ͽ��ν��αL�ͽ��ν��ν��ν��ν��ν��αL�ͽ��αL�ͽ��ν���=���=��ͽ��ν��αL�ͽ��ν��ν��ν��αL�ͽ��ν��αL�ͽ��ν��αL�ͱL�ͽ��ν��ν��αL�ͽ���>L��>L�ͱL�ͽ��αL�ͽ��ν��αL�ͱL�ͽ���=��ͽ��ν���=���=���=��ͽ��αL�ͽ��αL�ͱL�ͽ��ν��αL�ͱL�ͱL�ͽ��ν��ν��ν��αL��>L�ͱL�ͱL�ͱL�ͽ��ν��ν��ν��αL�ͽ��αL�ͽ��ν��ν��ν��ν��ν���=��ͽ��ν��ν��αL�ͽ��ν��αL��>L�ͽ��ν��ν��αL��=���=��ͽ��αL�ͱL�ͽ��ν��αL��=��ͽ��ν��ν���=���=��ͽ��ν��αL�ͽ��αL�ͽ���=���>���>L�ͽ��ν��αL�ͱL�ͽ��ν��αL��>L��>L�ͱL�ͽ��αL��=���>���>���=��ͽ���=��ͱL��=���>���>����L�ͽ��αL�ͱL�ͽ���>L�ͱL�ͽ��ν���=���>���>���>��ͱL�ͽ��αL�ͱL��=���>L�ͱL�ͽ��αL�ͽ���=��ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͽ��ν���=���>L�ͱL��>L�ͱL�ͱL��=���=���=���=��ͱL�ͱL��>L��>L��>L��>���>���>L��>���>���>L��>���>L��>L��>���>���>���=���=���>L��>L��>���>L��=���=���>���>L��>L��>L��=���=���>L��>L��>L��=���=���>���>L��=���>���>���>L�ͱL��>L��=���>L��>L��=���>L��>���=���>L��>L��=���=���=���>L��>L��>L��>���>L��>L��=���>L��>���>L��>���>L��>L��>L��>L��>L��=���>L��=���>L��>L��>���>L��>L��=���>L��=���=���=���>L��=���>L�ͱL��>���>L��>L��=���=���>L��>L��>���=���=���=���>L��>L��>L��>���>���=���>L��>L��=���>���=���>L��=���>L��=���>L��=���>���>L��>���>���>L��>L��>���>���>���>L��>L��>L��>L��>L��=���=��ͱL��>L��=���=���>L��>L��=���>L��>L��>���>L��>L��>���>L��>���>���>L��>L��>���>���>���>L��>L��>L��>L��>L��=���>L��>L��=���=���>L��=���>L��>L��>���=���>L��>L��>���>L��>L��>L��=���>L��>L��>L��>L��=���=���>L��>L��>L��>L��>���>���=���=���=���>L��>L��>L��>L��=���=���>L��>���>L��>���>���>L��=���>���>L��>L��>L��>L��>L��=���=���>���=���>L��>L��>L��=���=���>L��>L��>L��>L��>L��>L��>L��=���>L��>���>L��=��ͱL��>L��>L��=���>L��=���=���=���>L��>L��>���>L��>���>���>���?   ?   ?��?��?��?333?333?333?L��?L��?fff?fff?fff?�  ?���?���?���?���?���?�fg?�fg?�fg?�34?�34?�  ?�  ?�  ?���?���?���?ٙ�?�fg?�fg?�fg?�34?�34@   @ff@ff@ff@ff@��@��@33@33@33@��@��@   @   @&fg@,��@,��@333@333@9��@@  @Ffg@L��@L��@Y��@`  @`  @l��@l��@s33@y��@�  @�33@�33@�ff@���@���@�  @�  @�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@���@���@���@�  @�33@�ff@ə�@���@�  @�ff@�ff@���@�  @�33@�ff@陙@���@�33@�ff@���@���A   A33A��AffA  A	��A33AffAffA��A33A��AffA  A��A33A��AffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A8  A8  A;33A<��A>ffA@  AA��AD��AFffAH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A\��A`  Aa��Ad��AfffAi��Ak33Al��Ap  As33At��Ax  Ay��A|��A~ffA���A�fgA�34A���A���A�34A���A�fgA�34A���A���A�34A���A���A�34A�  A���A�34A�  A���A�34A�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�34A���A���A�34A���A���A�34A�  A���A�34A�  Ař�A�fgA�  A���A�fgA�  A���A�fgA�34A���Aљ�A�34A�  Aՙ�A�fgA�  A���A�fgA�34A�  Aݙ�Dp��Dp�3Dp��DqgDq�Dq�Dq  Dq&gDq33Dq9�DqFgDqL�DqY�Dq` DqfgDqs3Dqy�Dq�gDq��Dq�3Dq� Dq�gDq�3Dq��Dq�gDq��Dq�3Dq� Dq�gDq��Dq��Dr  Dr�Dr3Dr  Dr&gDr,�Dr9�Dr@ DrL�DrS3DrY�DrfgDrl�Dry�Dr� Dr��Dr�3Dr��Dr�gDr��Dr�3Dr� Dr�gDr�3DrٚDr� Dr��Dr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` DsfgDss3Dsy�Ds� Ds��Ds�3Ds� Ds�gDs��Ds��Ds� Ds�gDs�3DsٚDs�gDs��Ds�3Dt  DtgDt�Dt�Dt  Dt,�Dt33Dt9�DtFgDtL�DtY�Dt` DtfgDts3Dty�Dt� Dt��Dt�3Dt��Dt�gDt��Dt��Dt� Dt�gDt�3DtٚDt� Dt��Dt�3Du  DugDu�@9��@@  @Ffg@L��@L��@Y��@`  @`  @l��@l��@s33@y��@�  @�33@�33@�ff@���@���@�  @�  @�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@���@���@���@�  @�33@�ff@ə�@���@�  @�ff@�ff@���@�  @�33@�ff@陙@���@�33@�ff@���@���A   A33A��AffA  A	��A33AffAffA��A33A��AffA  A��A33A��AffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A8  A8  A;33A<��A>ffA@  AA��AD��AFffAH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A\��A`  Aa��Ad��AfffAi��Ak33Al��Ap  As33At��Ax  Ay��A|��A~ffA���A�fgA�34A���A���A�34A���A�fgA�34A���A���A�34A���A���A�34A�  A���A�34A�  A���A�34A�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�  A���A�fgA�34A���A���A�34A���A���A�34A�  A���A�34A�  Ař�A�fgA�  A���A�fgA�  A���A�fgA�34A���Aљ�A�34A�  Aՙ�A�fgA�  A���A�fgA�34A�  Aݙ�Dp��Dp�3Dp��DqgDq�Dq�Dq  Dq&gDq33Dq9�DqFgDqL�DqY�Dq` DqfgDqs3Dqy�Dq�gDq��Dq�3Dq� Dq�gDq�3Dq��Dq�gDq��Dq�3Dq� Dq�gDq��Dq��Dr  Dr�Dr3Dr  Dr&gDr,�Dr9�Dr@ DrL�DrS3DrY�DrfgDrl�Dry�Dr� Dr��Dr�3Dr��Dr�gDr��Dr�3Dr� Dr�gDr�3DrٚDr� Dr��Dr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` DsfgDss3Dsy�Ds� Ds��Ds�3Ds� Ds�gDs��Ds��Ds� Ds�gDs�3DsٚDs�gDs��Ds�3Dt  DtgDt�Dt�Dt  Dt,�Dt33Dt9�DtFgDtL�DtY�Dt` DtfgDts3Dty�Dt� Dt��Dt�3Dt��Dt�gDt��Dt��Dt� Dt�gDt�3DtٚDt� Dt��Dt�3Du  DugDu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��mA��A��mA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A�ĜAȶFAȮA�dZA�VAŸRA�S�A�ZA�\)A��A�=qA�r�A�ĜA��A�l�A���A�1'A���A�bNA�S�A�?}A�%A�t�A�1'A�JA��A�=qA���A��HA��uA���A�9XA��FA���A��-A��7A�t�A�O�A��A�7LA�%A��
A���A��7A�$�A��^A�&�A���A�Q�A��;A�p�A�v�A���A��PA��A�VA�`BA���A�/A�ȴA�$�A�n�A�v�A�XA��\A���A�p�A�+A�p�A���A��HA���A�l�A�l�A���A�ZA��A��A��A�JA��^A�bNA�7LA��hA��TA�v�A��TA�9XA�XA���A�G�A� �A��TA���A� �A�z�A�{A�|�A~v�A|JA{\)Azr�AxE�At�As/AqAo
=Ah{Ac��Aa��A`��A_�A^��A^5?A]��A\ZAX��AVQ�AV$�AV{AS�#AP��AOC�AN��AN1'AM�AKx�AJz�AHI�AG�7AF�DAD��AChsAA��A@�DA>I�A<�A;A:�A9��A9"�A8-A7;dA6�A4n�A2�`A1�A.ȴA-+A,JA)�FA(^5A%�^A#A"�A"-A!�;A �\A�PA��A�A��A�7A��AAG�A
=A��AĜAp�A��A��A��A~�A
=A��A�AK�A�yA�A�A
��A
JA	\)A��AA%A�Al�A
=A�+AJA;dA��AffA�hA �/A M�@�33@�E�@���@��@��D@��\@��9@��w@���@���@�@�u@�@�r�@�dZ@���@��@�O�@��@�b@畁@�"�@�+@㕁@ޗ�@�X@ܴ9@�
=@�-@��T@١�@�j@��
@�dZ@���@և+@�v�@Լj@�  @�33@�+@ҸR@�?}@�dZ@�E�@�x�@��@���@�l�@��H@ʟ�@�@�(�@�{@���@��;@�C�@�+@��@�@�ff@���@��-@��@�ƨ@�E�@�j@��
@�x�@�%@���@�@��@��@���@�bN@�r�@��@�-@��!@��w@�|�@�K�@��R@�M�@�5?@��T@���@���@���@�l�@�S�@���@�{@��^@�hs@�/@��@�1'@���@���@���@�Ĝ@��w@���@��7@��h@���@��7@�G�@��@�V@���@��D@�I�@� �@�ƨ@�33@��@�M�@��@��@�hs@��`@�9X@�  @��P@�dZ@�+@���@�ff@�n�@�E�@��@�X@��`@��j@�r�@�A�@��
@�t�@�S�@�
=@���@�V@�$�@��@��^@���@��h@��-@��@�5?@��T@�V@��j@�A�@��@��@�dZ@�"�@���@���@�@���@��H@��+@�V@�$�@�{@���@�x�@�G�@�7L@��9@�1'@�1@���@�;d@�33@��@�~�@�M�@��T@�X@�/@�&�@�/@�/@�7L@�/@��j@�b@��
@���@�"�@��H@���@�v�@��+@��\@�ff@�{@��-@���@�hs@�/@�%@���@�Ĝ@��@��D@�j@�9X@��@��;@�ƨ@���@�|�@�C�@��@�v�@�=q@�$�@���@�@���@�X@�/@��@�%@��`@�Ĝ@���@�z�@�A�@��@���@��@�dZ@�+@��@�ȴ@�v�@�^5@�@���@�p�@�`B@�?}@���@�z�@�A�@�  @��m@��w@���@�l�@���@���@���@��+@���@�=q@��@���@���@��@�x�@C@v�1@n\�@f8�@^�@V�6@N��@Ij@A�@:�R@5O�@.�s@*L0@$�@v�@($@��@��@r�@�@	c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�v�A���A�5?AȶFA�A�=qA���A���A�oA�G�AȓuA�O�A�+A�\)AȼjA�jA��#A���A�oAȾwA���A��Aƴ9A��A�r�A���A���A�bNA��wA�(�A�|�A���A�%Aț�A�+A�hsA���A��PA��jA��A�z�A�hsA�A�/A�33AȺ^AȺ^A��/A���AȸRA���A�ȴA��`A�+A��yA�{A��mA�?}A��A���A�jAȧ�A�E�A�M�A�9XAÍPA�  AÃAȶFAȃA��A�Q�A�`BA�M�A�1'Aȣ�A�\)AœuAȋDA��wA��
Aȥ�Aș�A��A�(�A�ffA��A�r�A�;dA�n�A�ƨA�M�A�ĜA�oA��RA�hsA���A°!Aȥ�Aȏ\A�G�A��A��A� �A�Q�A�l�A��A�XA���A�&�A�l�A��A���A���A��+A�
=A�  A�JA�-A�"�A��wA��A�AȲ-A�$�A��A���A�ffA���Aȧ�A�M�A�/Aȥ�A� �A�A���AȾwA�C�A��A��A°!Aȧ�A�x�A�5?A���A��PAżjA�C�A�oAȰ!AȲ-A7A��#A�`BA�K�A���A��Aß�A�I�Aȴ9Aț�A��A���Aȧ�AȲ-AȶFAȮA���Aò-A�XAȺ^AȺ^AȾwAȩ�A��A�"�A�K�A��Aƙ�Aȇ+A��/Að!A��A�ƨA�A�ĜAȩ�A��A���A�dZA��AȾwAȥ�A�XA®A�ffAƬA�AȺ^AȬAĉ7A��#Aŉ7A�XAȸRAȬA¶FAȏ\Aȏ\A���AȰ!Aȥ�AȼjAȸRA�ȴA�n�AȸRAȼjAȸRA�|�A�?}A���A�ĜA�A�A�ĜA�ĜA�ĜA�A�ƨA�A���AȶFAȾwAȾwAȼjAȼjAȴ9AȸRAȸRAȼjAȾwAȾwAȺ^AȼjA���Aȴ9AȸRAȶFAȬAȸRA�ĜA�ĜA�A���A�x�A�ĜAȾwAȼjA�ƨA�ƨA���A���A�ĜAȶFA���A�ȴA���A���A�ȴA���A���A�ƨA�A���A�ĜA���A�ƨA���A�ȴA���A�ĜA���A�ȴA���A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ȴA�ĜAȾwAȼjA�ĜA�ȴA���A�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ĜAȾwA���A�ƨA�ĜA���A���A�ȴA�ȴA�ĜA�A�ĜA�A���AȺ^AȾwA�ƨA�ȴAȸRA���A���A�ƨA�ȴA�A�ƨA�ȴA���A�AȾwAȺ^AȼjA���A���A�AȾwA�ȴA�ƨA���A�ƨA���A�ƨA�ȴA�ƨA�ĜAȺ^A���A�A�AœuAȺ^AȺ^AȾwA�A�ĜAȼjAȴ9A�A�ĜA�ĜA�ƨA�ĜA�A���A���A�A���A�A�ȴA���A�ȴA���A�ƨA���A���A���A���A�ƨA�ȴA�ȴA���A�ƨA�ȴA�ĜA�A�ƨA�ȴA���A���A�ȴA�A���A�ȴA���A���A�ȴA���A�ȴA�ĜA�ĜA�ĜA���A���A���A���A���A���A���A���A���A�ƨA���A���A���A�ȴA���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ƨA���A��
A���A���A�ȴA���A���A�ȴA���A���A�ƨAȕ�A���A���A�ƨA���A���A�ƨA���A���A���A��
A���A���A���A��;A���A��#A��HA��HA��`A��`A��TA��TA��`A��HA��HA��TA��`A��TA��HA��`A��`A��#A��;A��HA��#A��#A��/A��/A��A��
A��/A��HA��#A��HA��;A���A���A���A���A��
A��
A���A��
A���A��#A���A��HA��TA��TA��;A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��A��yA��A��A��A��yA��yA��mA��`A��mA��`A��mA��mA��mA��mA��`A��`A��`A��mA��mA��yA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��mA��`A��mA��mA��mA��`A��TA��`A��yA��mA��`A��mA��`A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��`A��TA��`A��mA��TA��A���A���A��`A��yA��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A��A���A��A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���@���@���@���@���@���@���@���@���@��\@���@���@��\@��+@��+@��+@��+@�~�@�~�@��+@��+@��+@��+@��+@��+@��\@���@���@��!@��!@��R@��R@��R@��R@��!@���@���@���@���@��\@��+@�v�@�ff@�^5@�V@�V@�M�@�M�@�5?@��@�{@�J@�@���@���@���@���@���@���@���@���@��@��@��@��T@��T@���@���@���@���@���@���@�@�@�@�@�@�@���@�@�@�@�@���@���@�@��^@��-@���@���@���@���@���@��h@��h@��h@��7@��7@��@��7@��@��@��@��@��@��@�x�@��@��@��@��@��@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�@�x�A��A��A��A��A��yA��A��yA��A��A��A��A��mA��mA��mA��mA��mA��`A��mA��mA��`A��`A��`A��`A��`A��`A��TA��TA��TA��mA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��yA��mA��mA��TA��`A��HA��TA��`A��TA��TA��HA��HA��A��A��A��A���A��A��A��A���A���A��A��yA��TA��`A��`A��mA��A���A���A��mA��mA��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��\@���@���@���@��!@��R@��R@���@��R@��R@��!@���@���@���@��\@��\@��+@�v�@�ff@�^5@�V@�V@�M�@�=q@�$�@��@�J@�J@�@���@���@���@���@���@���@���@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@�@�@�@�@�@�@���@���@�@���@���@���@�@��^@���@���@���@���@���@���@���@��h@��h@��7@��7@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�x�@��@��@�x�@��@�x�@�x�@�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A��A��mA��A��mA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A�ĜAȶFAȮA�dZA�VAŸRA�S�A�ZA�\)A��A�=qA�r�A�ĜA��A�l�A���A�1'A���A�bNA�S�A�?}A�%A�t�A�1'A�JA��A�=qA���A��HA��uA���A�9XA��FA���A��-A��7A�t�A�O�A��A�7LA�%A��
A���A��7A�$�A��^A�&�A���A�Q�A��;A�p�A�v�A���A��PA��A�VA�`BA���A�/A�ȴA�$�A�n�A�v�A�XA��\A���A�p�A�+A�p�A���A��HA���A�l�A�l�A���A�ZA��A��A��A�JA��^A�bNA�7LA��hA��TA�v�A��TA�9XA�XA���A�G�A� �A��TA���A� �A�z�A�{A�|�A~v�A|JA{\)Azr�AxE�At�As/AqAo
=Ah{Ac��Aa��A`��A_�A^��A^5?A]��A\ZAX��AVQ�AV$�AV{AS�#AP��AOC�AN��AN1'AM�AKx�AJz�AHI�AG�7AF�DAD��AChsAA��A@�DA>I�A<�A;A:�A9��A9"�A8-A7;dA6�A4n�A2�`A1�A.ȴA-+A,JA)�FA(^5A%�^A#A"�A"-A!�;A �\A�PA��A�A��A�7A��AAG�A
=A��AĜAp�A��A��A��A~�A
=A��A�AK�A�yA�A�A
��A
JA	\)A��AA%A�Al�A
=A�+AJA;dA��AffA�hA �/A M�@�33@�E�@���@��@��D@��\@��9@��w@���@���@�@�u@�@�r�@�dZ@���@��@�O�@��@�b@畁@�"�@�+@㕁@ޗ�@�X@ܴ9@�
=@�-@��T@١�@�j@��
@�dZ@���@և+@�v�@Լj@�  @�33@�+@ҸR@�?}@�dZ@�E�@�x�@��@���@�l�@��H@ʟ�@�@�(�@�{@���@��;@�C�@�+@��@�@�ff@���@��-@��@�ƨ@�E�@�j@��
@�x�@�%@���@�@��@��@���@�bN@�r�@��@�-@��!@��w@�|�@�K�@��R@�M�@�5?@��T@���@���@���@�l�@�S�@���@�{@��^@�hs@�/@��@�1'@���@���@���@�Ĝ@��w@���@��7@��h@���@��7@�G�@��@�V@���@��D@�I�@� �@�ƨ@�33@��@�M�@��@��@�hs@��`@�9X@�  @��P@�dZ@�+@���@�ff@�n�@�E�@��@�X@��`@��j@�r�@�A�@��
@�t�@�S�@�
=@���@�V@�$�@��@��^@���@��h@��-@��@�5?@��T@�V@��j@�A�@��@��@�dZ@�"�@���@���@�@���@��H@��+@�V@�$�@�{@���@�x�@�G�@�7L@��9@�1'@�1@���@�;d@�33@��@�~�@�M�@��T@�X@�/@�&�@�/@�/@�7L@�/@��j@�b@��
@���@�"�@��H@���@�v�@��+@��\@�ff@�{@��-@���@�hs@�/@�%@���@�Ĝ@��@��D@�j@�9X@��@��;@�ƨ@���@�|�@�C�@��@�v�@�=q@�$�@���@�@���@�X@�/@��@�%@��`@�Ĝ@���@�z�@�A�@��@���@��@�dZ@�+@��@�ȴ@�v�@�^5@�@���@�p�@�`B@�?}@���@�z�@�A�@�  @��m@��w@���@�l�@���@���@���@��+@���@�=q@��@���@���@��G�O�@C@v�1@n\�@f8�@^�@V�6@N��@Ij@A�@:�R@5O�@.�s@*L0@$�@v�@($@��@��@r�@�@	c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�v�A���A�5?AȶFA�A�=qA���A���A�oA�G�AȓuA�O�A�+A�\)AȼjA�jA��#A���A�oAȾwA���A��Aƴ9A��A�r�A���A���A�bNA��wA�(�A�|�A���A�%Aț�A�+A�hsA���A��PA��jA��A�z�A�hsA�A�/A�33AȺ^AȺ^A��/A���AȸRA���A�ȴA��`A�+A��yA�{A��mA�?}A��A���A�jAȧ�A�E�A�M�A�9XAÍPA�  AÃAȶFAȃA��A�Q�A�`BA�M�A�1'Aȣ�A�\)AœuAȋDA��wA��
Aȥ�Aș�A��A�(�A�ffA��A�r�A�;dA�n�A�ƨA�M�A�ĜA�oA��RA�hsA���A°!Aȥ�Aȏ\A�G�A��A��A� �A�Q�A�l�A��A�XA���A�&�A�l�A��A���A���A��+A�
=A�  A�JA�-A�"�A��wA��A�AȲ-A�$�A��A���A�ffA���Aȧ�A�M�A�/Aȥ�A� �A�A���AȾwA�C�A��A��A°!Aȧ�A�x�A�5?A���A��PAżjA�C�A�oAȰ!AȲ-A7A��#A�`BA�K�A���A��Aß�A�I�Aȴ9Aț�A��A���Aȧ�AȲ-AȶFAȮA���Aò-A�XAȺ^AȺ^AȾwAȩ�A��A�"�A�K�A��Aƙ�Aȇ+A��/Að!A��A�ƨA�A�ĜAȩ�A��A���A�dZA��AȾwAȥ�A�XA®A�ffAƬA�AȺ^AȬAĉ7A��#Aŉ7A�XAȸRAȬA¶FAȏ\Aȏ\A���AȰ!Aȥ�AȼjAȸRA�ȴA�n�AȸRAȼjAȸRA�|�A�?}A���A�ĜA�A�A�ĜA�ĜA�ĜA�A�ƨA�A���AȶFAȾwAȾwAȼjAȼjAȴ9AȸRAȸRAȼjAȾwAȾwAȺ^AȼjA���Aȴ9AȸRAȶFAȬAȸRA�ĜA�ĜA�A���A�x�A�ĜAȾwAȼjA�ƨA�ƨA���A���A�ĜAȶFA���A�ȴA���A���A�ȴA���A���A�ƨA�A���A�ĜA���A�ƨA���A�ȴA���A�ĜA���A�ȴA���A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ȴA�ĜAȾwAȼjA�ĜA�ȴA���A�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ĜAȾwA���A�ƨA�ĜA���A���A�ȴA�ȴA�ĜA�A�ĜA�A���AȺ^AȾwA�ƨA�ȴAȸRA���A���A�ƨA�ȴA�A�ƨA�ȴA���A�AȾwAȺ^AȼjA���A���A�AȾwA�ȴA�ƨA���A�ƨA���A�ƨA�ȴA�ƨA�ĜAȺ^A���A�A�AœuAȺ^AȺ^AȾwA�A�ĜAȼjAȴ9A�A�ĜA�ĜA�ƨA�ĜA�A���A���A�A���A�A�ȴA���A�ȴA���A�ƨA���A���A���A���A�ƨA�ȴA�ȴA���A�ƨA�ȴA�ĜA�A�ƨA�ȴA���A���A�ȴA�A���A�ȴA���A���A�ȴA���A�ȴA�ĜA�ĜA�ĜA���A���A���A���A���A���A���A���A���A�ƨA���A���A���A�ȴA���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ƨA���A��
A���A���A�ȴA���A���A�ȴA���A���A�ƨAȕ�A���A���A�ƨA���A���A�ƨA���A���A���A��
A���A���A���A��;A���A��#A��HA��HA��`A��`A��TA��TA��`A��HA��HA��TA��`A��TA��HA��`A��`A��#A��;A��HA��#A��#A��/A��/A��A��
A��/A��HA��#A��HA��;A���A���A���A���A��
A��
A���A��
A���A��#A���A��HA��TA��TA��;A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��A��A��A��A��mA��mA��mA��mA��mA��`A��mA��mA��`A��`A��`A��`A��`A��`A��TA��TA��TA��mA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��yA��mA��mA��TA��`A��HA��TA��`A��TA��TA��HA��HA��A��A��A��A���A��A��A��A���A���A��A��yA��TA��`A��`A��mA��A���A���A��mA��mA��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��\@���@���@���@��!@��R@��R@���@��R@��R@��!@���@���@���@��\@��\@��+@�v�@�ff@�^5@�V@�V@�M�@�=q@�$�@��@�J@�J@�@���@���@���@���@���@���@���@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@�@�@�@�@�@�@���@���@�@���@���@���@�@��^@���@���@���@���@���@���@���@��h@��h@��7@��7@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�x�@��@��@�x�@��@�x�@�x�@�x�A��A��A��A��A��yA��A��yA��A��A��A��A��mA��mA��mA��mA��mA��`A��mA��mA��`A��`A��`A��`A��`A��`A��TA��TA��TA��mA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��yA��mA��mA��TA��`A��HA��TA��`A��TA��TA��HA��HA��A��A��A��A���A��A��A��A���A���A��A��yA��TA��`A��`A��mA��A���A���A��mA��mA��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��+@��\@���@���@���@��!@��R@��R@���@��R@��R@��!@���@���@���@��\@��\@��+@�v�@�ff@�^5@�V@�V@�M�@�=q@�$�@��@�J@�J@�@���@���@���@���@���@���@���@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@�@�@�@�@�@�@���@���@�@���@���@���@�@��^@���@���@���@���@���@���@���@��h@��h@��7@��7@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�x�@��@��@�x�@��@�x�@�x�@�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��>q��=�|>�m	@�P�=�Jb>t�@!f'@�E$=���?I�@�T=�/>u:@R��@�M�>��P?l�=㍤>��<@�>@�K�=��>�}�@h�r=��U>:�
=��<=��=Ӯh>\h@8�@�D�>'(@�T"=�I�=��&=�=��>��>_?�a>��>�g>ʆ�>m�@���@�B�?�P3>:��@���?�3r=��?�=�>NX@�=�=�d>�s�@�H�>H�?}f'@�Ex=�=�Go=��->]�k=���>e3�@�D�@�J�>��@f��?3�1=۠�>+�@�??2y�>�pz@�>l=�t�>_�j@�:�@�C�>�5�>��?ӌ�?[�>"w�@��==�m>1.�@�8	@�k�=�8�=���?��%?x>W�@���@��>Z�?�f@�=>�m=��=�(?*'@�=\>S��@�QY=U{�=f�3=��=�K^=�à?.#�@�M�=�3�@CW>H�%>��=�)t>u��@�Ex@�I�=��L=�s�>�1'?�V@�I?�;�>z�@�J#=�Y�=�ag>PH@�L�@�F�=���=�A_>�/E@�Ex@���>U�?w�N?ax�?G�6?��u>J�@�HV@�K�@��>�A@�_�@�d�@z�=���=�S�@�~@�F5@�I�?G�>:�@�I�@�I@�I@�MU=��>fa(@�N�@�N@�M�@�L�@�P�=�۶?�4�?}.s?�AJ?��@�O�=�u�=꣭>@Ĝ@�RT@�R�@�R�@�T"=��t>V �@���@�Ri@�QD@�Ov>��?W�w@%�:?�z�@�S@�RT@�AJ?�n�@�B�>��@��@�L�@�LD?6�@�%@�p?�O�@�L�@�P�@�T@�Se?�c@��@�P�@�P�@�O@�@:@�Q�@�S�@�Tv@�T�@�T"@�U2@�U2@�Ta@�T"@�T"@�S�@�Tv@�QD@�R�@�RT@�Q�@�P3@�P3@�O�@�Q/@�Q�@�R�@�Q�@�RT@�R�@�R?@�P�@�Ov@�O"@�N�@�Ta@�T�@�Tv@�T"@�Se@���@�T�@�U�@�U2@�V�@�VC@�Uq@�T"@�R�@�RT@�Xd@�V�@�V�@�WT@�W�@�WT@�V�@�U�@�V�@�VC@�U�@�WT@�VC@�V�@�W�@�W�@�U�@�U�@�V�@�W�@�U2@�WT@�VC@�U2@�U�@�V.@�U�@�U�@�V�@�VC@�V�@�V.@�V�@�WT@�WT@�V�@�V�@�X:@�U�@�U�@�VC@�V�@�U2@�T@�W�@�V�@�V�@�VC@�V�@�Uq@�U�@�VC@�T"@�P3@�U2@�U�@�U�@�U�@�V�@�WT@�W�@�W�@�U2@�W�@�W�@�Tv@�U�@�R�@�R�@�Se@�WT@�V�@�U2@�U@�V�@�U2@�U�@�V�@�Y@�W*@�V�@�U2@�U2@�U�@�S@�SP@�Q�@��Z@�RT@�QD@�P�@�T�@�W�@�R�@�Se@�U2@�V�@�V�@�VC@�Ta@�Xd@�W�@�W�@�T"@�S�@�U�@�T�@�T�@�WT@�W�@�X�@�Y�@�Xd@�Yu@�V�@�Tv@�U�@�Uq@�V�@�S@�S@�T"@�U�@�U@�V�@�U�@�U2@�T�@�Ta@�T�@�V�@�Xd@�U�@�VC@�V�@�U�@�T�@�V�@�W�@�Xd@�X�@�X�@�Y@�W�@�W�@�Xd@�YK@�W�@�W�@�W�@�Y@�V�@�X�@�X:@�Y@�Yu@�X�@�WT@�V�@�X�@�Y@�Y@�Y@�W�@�W�@�X�@�W�@�Yu@�Y@�Yu@�X:@�W�@�Yu@�Z@�X�@�Yu@�Z�@�[@�Y�@�X�@�Xd@�Y@�Z@�[@�Z�@�Yu@�Y�@�Z@�Z�@�Xd@�Z@�YK@�VC@�Y@�Y�@�Z@�[�@�[�@�\�@�^@�]�@�`-@�`�@�a@�`�@�a=@�a=@�a�@�bN@�a@�a@�a=@�bN@�a�@�a=@�`�@�`�@�_�@�^t@�_�@�`-@�^�@�^�@�^t@�^@�^t@�_@�_�@�_�@�^@�^@�\>@�[�@�[�@�\)@�\�@�^@�^�@�^@�_�@�`k@�b�@�cs@�cs@�b�@�c�@�eA@�fQ@�f�@�e�@�e�@�fQ@�fQ@�e�@�fQ@�e�@�e,@�e�@�e,@�d�@�d�@�d�@�e�@�e�@�e@�d�@�d�@�d�@�dE@�c�@�c�@�c�@�c�@�c�@�c�@�d0@�d0@�c�@�d�@�d�@�e�@�ff@�fQ@�f�@�g@�f@�g@�g@�gb@�gw@�g�@�gb@�g�@�gw@�h@�g�@�gb@�gb@�g�@�g�@�h@�g�@�h4@�g�@�f�@�fQ@�f@�fQ@�fQ@�fQ@�f�@�f�@�e�@�e�@�e�@�ff@�ff@�e�@�e�@�ff@�g�@�h�@�j�@�j�@�i�@�i�@�i�@�i/@�i�@�i�@�j�@�i�@�i�@�h�@�gw@�g�@�hI@�h�@�jU@�l"@�m�@�i�@�iD@�k�@�jU@�n/@�o�@�p@�pe@�p@�p@�p@�o�@�oT@�oT@�oT@�o @�n�@�nD@�n�@�o @�o @�o @�o @�oT@�o @�o @�o @�oT@�oT@�oT@�o @�oT@�o @�oT@�o�@�p@�pz@�p@�oT@�o�@�oi@�p@�p@�o�@�p@�p@�p@�pz@�p&@�oi@�oi@�o�@�o�@�p&@�o�@�pz@�p�@�pz@�q7@�q7@�p�@�q7@�q7@�q7@�p�@�pz@�p�@�p�@�q�@�rG@�q�@�p�@�q7@�q7@�q7@�q�@�rG@�rG@�rG@�q�@�qL@�q�@�q�@�rG@�q�@�rG@�s@�s�@�t@�t@�sm@�s@�sX@�s�@�t*@�t�@�t�@�uy@�u�@�u�@�vK@�v�@�vK@�v�@�v�@�w@�v�@�v�@P��@P��@P��@P�%@P�%@P��@P��@P�%@P��@P��@P��@P�*@P��@P��@P��@P��@P��@P��@P�S@P�}@P��@P�O@P��@P��@P�l@P�@P��@P�[@P�,@P��@P�@P��@P�@P�@P��@P��@P�p@P�y@P�}@P�\@P��@P��@P�L@P�Q@P�U@P�Y@P��@P��@P~|@P}�@P|�@P{�@P{�@P{�@P{5@P{@P{5@Pz�@Pz�@Py�@Pyh@Py@Pw�@Pv�@PvK@Pu�@Pu%@Pt�@PtT@Pt*@PtT@PtT@Pt*@Pt @Pt @PtT@PtT@Pt�@Pt~@Pt�@Pt�@Pu�@Pv�@Pv!@Pt�@Ps�@Pr�@Pr@Pq@Pp�@Pp�@Pp@Pp@Pp@Poi@Pn�@Pnn@Pnn@Pn@Pn@Pn@Pnn@Pnn@Pnn@Pnn@Pn@Pnn@Pn@Pn@Pn�@Pn�@Pn@Pnn@Pnn@Pn�@Pn�@Pnn@Pn�@Pnn@Pn�@��^@���@���@���@���@���@��|@���@��9@��c@��N@���@���@��k@���@���@��,@��A@��k@��A@��k@��@��A@��,@��k@��@���@���@���@���@���@���@���@��@��0@���@��E@��E@��E@��^@��
@���@���@��o@��k@���@��I@���@��0@���@���@��9@��x@��x@���@��9@��9@��(@��|@���@���@���@���@���@���@���@��o@���@��s@��@���@��<@��w@���@��/@��w@���@���@��9@���@���@���@��@���@��@��0@���@���@���@��@@���@��Q@��v@��a@���@��7@��7@���@���@��z@��'@���@��@@���@���@���@��'@���@���@��<@���@���@��'@��e@��<@��<@��'@��Q@��<@��z@���@��a@���@���@��<@��e@���@���@��"@���@���@��7@���@���@��@��Q@���@���@���@���@��7@��7@��a@���@���@��a@��a@��L@��	@���@���@���@��a@��	@���@���@���@���@���@���@��3@���@��]@���@��	@��L@���@���@��L@��"@���@���@��H@���@��*@���@���@��H@��@���@��@��@��&@���@���@���@��K@���@���@��K@���@���@P�v@P�"@P�L@P�L@P�v@P�v@P�"@P�L@P�"@P�L@P�"@P�"@P�'@P��@P��@P�+@P�U@P�@P�@P��@P��@P�P@P¤@P��@P�v@P�m@Pɰ@Pɰ@P�S@P̣@P�@P�F@P��@P�!@P�y@P�)@Pʂ@P�@PƓ@P�?@PĜ@P�'@P�4@P�<@P��@P�o@P��@P��@P�@P�p@P�y@P�%@P��@P�.@P�@P��@P��@P�@P��@P��@P�\@P��@P�@P�i@P��@P��@P��@P�v@P��@P��@P�z@P�'@P�'@P��@P��@P��@P�'@P��@P��@P��@P��@P�"@P��@P�@P�@P��@P��@P�+@P�@P��@P�
@P�4@P��@P�@P��@P��@P�k@P�k@P�o@P��@P��@P��@P��@P��@P�A@P�@P��@P��@P��@P�@P��@P��@P��@P��@P�@P�@P��@P�@P��@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   44443444344344334444334434444444343444444444443344344444344344344444433434443443443344444344334444433443444434344444434444443344443443444334443344444433443334443344333344333334444434443333443333444433343433343343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�P�G�O�G�O�G�O�@�E G�O�G�O�@�TG�O�G�O�@R��@�M�G�O�G�O�G�O�G�O�@�>	@�K�G�O�G�O�@h�pG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�D�G�O�@�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�B�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�@�=�G�O�G�O�@�H�G�O�G�O�@�E|G�O�G�O�G�O�G�O�G�O�G�O�@�D�@�J�G�O�@f��G�O�G�O�G�O�@�?G�O�G�O�@�>lG�O�G�O�@�:�@�C�G�O�G�O�G�O�G�O�G�O�@��?G�O�G�O�@�8@�k�G�O�G�O�G�O�G�O�G�O�@���@��G�O�G�O�@�=G�O�G�O�G�O�G�O�@�=\G�O�@�Q]G�O�G�O�G�O�G�O�G�O�G�O�@�M�G�O�G�O�G�O�G�O�G�O�G�O�@�E{@�I�G�O�G�O�G�O�G�O�@�IG�O�G�O�@�J"G�O�G�O�G�O�@�L�@�F�G�O�G�O�G�O�@�Ev@���G�O�G�O�G�O�G�O�G�O�G�O�@�HU@�K�G�O�G�O�@�_�@�d�@z�G�O�G�O�G�O�@�F6@�I�G�O�G�O�@�I�@�I@�I@�MYG�O�G�O�@�N�@�N@�M�@�L�@�P�G�O�G�O�G�O�G�O�G�O�@�O�G�O�G�O�G�O�@�RQ@�R�@�R�@�T'G�O�G�O�@���@�Rj@�QC@�OrG�O�G�O�G�O�G�O�@�S@�R[@�AJG�O�@�B�G�O�@��@�L�@�LEG�O�@�%!@�tG�O�@�L�@�P�@�T@�SdG�O�@��@�P�@�P�@�O@�@:@�Q�@�S�@�Tz@�T�@�T"@�U6@�U6@�Ta@�T@�T@�S�@�Tz@�QH@�R�@�RV@�Q�@�P/@�P3@�O�@�Q.@�Q�@�R�@�Q�@�RX@�R�@�RB@�P�@�Ox@�O@�N�@�Tc@�T�@�Tz@�T"@�Sd@���@�T�@�U�@�U0@�V�@�VG@�Ur@�T&@�R�@�RV@�Xh@�V�@�V�@�WQ@�W�@�WV@�V�@�U�@�V�@�V>@�U�@�WR@�VB@�V�@�W�@�W�@�U�@�U�@�V�@�W�@�U0@�WS@�VB@�U1@�U�@�V,@�U�@�U�@�V�@�V?@�V�@�V,@�V�@�WR@�WV@�V�@�V�@�X:@�U�@�U�@�VF@�V�@�U1@�T@�W�@�V�@�V�@�VB@�V�@�Ur@�U�@�VE@�T"@�P2@�U4@�U�@�U�@�U�@�V�@�WT@�W�@�W�@�U3@�W�@�W�@�Tt@�U�@�R�@�R�@�Se@�WQ@�V�@�U6@�U@�V�@�U2@�U�@�V�@�Y@�W*@�V�@�U1@�U0@�U�@�S@�SL@�Q�@��[@�R[@�QC@�P�@�T�@�W�@�R�@�Sd@�U3@�V�@�V�@�VB@�Ta@�Xi@�W�@�W�@�T@�S�@�U�@�T�@�T�@�WV@�W�@�X�@�Y�@�Xa@�Yp@�V�@�Tt@�U�@�Ur@�V�@�S@�S@�T"@�U�@�U @�V�@�U�@�U/@�T�@�Ta@�T�@�V�@�Xb@�U�@�VB@�V�@�U�@�T�@�V�@�W�@�Xa@�X�@�X�@�Y@�W�@�W�@�Xb@�YM@�W�@�W�@�W�@�Y@�V�@�X�@�X<@�Y@�Yt@�X�@�WT@�V�@�X�@�Y	@�Y
@�Y	@�W�@�W�@�X�@�W�@�Yv@�Y@�Yz@�X=@�W�@�Yv@�Z!@�X�@�Yw@�Z�@�[@�Y�@�X�@�Xh@�Y
@�Z@�[@�Z�@�Yt@�Y�@�Z"@�Z�@�Xh@�Z@�YJ@�VA@�Y@�Y�@�Z@�[�@�[�@�\�@�^@�]�@�`2@�`�@�a@�`�@�a>@�a>@�a�@�bM@�a@�a@�aA@�bM@�a�@�a;@�`�@�`�@�_�@�^v@�_�@�`+@�^�@�^�@�^t@�^
@�^q@�_@�_�@�_�@�^@�^@�\=@�[�@�[�@�\*@�\�@�^
@�^�@�^@�_�@�`n@�b�@�ct@�cs@�b�@�c�@�eE@�fP@�f�@�e�@�e�@�fR@�fQ@�e�@�fQ@�e�@�e+@��]@���@���@���@���@���@��{@���@��;@��b@��N@���@��@��n@���@���@��.@��G@��j@��B@��k@��@��?@��+@��m@��@���@���@���@���@���@���@���@��@��/@���@��B@��E@��H@��`@��
@���@���@��l@��m@���@��G@���@��/@���@���@��:@��v@��v@���@��:@��8@��&@��{@���@���@���@���@���@���@���@��l@���@��r@��@���@��?@��y@���@��.@��w@���@���@��9@���@���@���@��@���@��~@��1@���@���@���@��?@���@��N@��t@��b@���@��6@��8@���@���@��z@��(@���@��B@���@���@���@��(@���@���@��;@���@���@��&@��f@��;@��:@��+@��R@��;@��y@���@��a@���@���@��:@��b@���@���@��$@���@���@��6@���@���@��@��S@���@���@���@���@��3@��8@��_@���@���@��_@��d@��K@��@���@���@���@��_@��@���@���@���@���@���@���@��5@���@��Z@���@��@��O@���@���@��O@��'@���@���@��H@���@��)@���@���@��J@��@���@��@��@��"@���@���@���@��J@���@���@��J@���@���@P�v@P�%@P�M@P�R@P�s@P�z@P�"@P�M@P�@P�M@P�"@P�#@P�%@P��@P��@P�.@P�S@P��@P�}@P��@P��@P�M@P¦@P��@P�}@P�n@Pɲ@Pɰ@P�R@P̠@P� @P�C@P��@P� @P�x@P�*@Pʂ@P�
@PƘ@P�>@Pĝ@P�(@P�5@P�;@P��@P�r@P��@P��@P�@P�r@P�x@P� @P��@P�.@P�@P��@P��@P�@P��@P��@P�^@P��@P�@P�f@P��@P��@P��@P�v@P��@P��@P�{@P�(@P� @P��@P��@P��@P�(@P��@P��@P��@P��@P�%@P��@P�@P�@P��@P��@P�.@P�@P��@P�@P�5@P��@P�@P��@P��@P�n@P�n@P�n@P��@P��@P��@P��@P��@P�B@P�@P��@P��@P��@P�@P��@P��@P��@P��@P�@P�@P��@P�@P��@P�@��]@���@���@���@���@���@��{@���@��;@��b@��N@���@��@��n@���@���@��.@��G@��j@��B@��k@��@��?@��+@��m@��@���@���@���@���@���@���@���@��@��/@���@��B@��E@��H@��`@��
@���@���@��l@��m@���@��G@���@��/@���@���@��:@��v@��v@���@��:@��8@��&@��{@���@���@���@���@���@���@���@��l@���@��r@��@���@��?@��y@���@��.@��w@���@���@��9@���@���@���@��@���@��~@��1@���@���@���@��?@���@��N@��t@��b@���@��6@��8@���@���@��z@��(@���@��B@���@���@���@��(@���@���@��;@���@���@��&@��f@��;@��:@��+@��R@��;@��y@���@��a@���@���@��:@��b@���@���@��$@���@���@��6@���@���@��@��S@���@���@���@���@��3@��8@��_@���@���@��_@��d@��K@��@���@���@���@��_@��@���@���@���@���@���@���@��5@���@��Z@���@��@��O@���@���@��O@��'@���@���@��H@���@��)@���@���@��J@��@���@��@��@��"@���@���@���@��J@���@���@��J@���@���@P�v@P�%@P�M@P�R@P�s@P�z@P�"@P�M@P�@P�M@P�"@P�#@P�%@P��@P��@P�.@P�S@P��@P�}@P��@P��@P�M@P¦@P��@P�}@P�n@Pɲ@Pɰ@P�R@P̠@P� @P�C@P��@P� @P�x@P�*@Pʂ@P�
@PƘ@P�>@Pĝ@P�(@P�5@P�;@P��@P�r@P��@P��@P�@P�r@P�x@P� @P��@P�.@P�@P��@P��@P�@P��@P��@P�^@P��@P�@P�f@P��@P��@P��@P�v@P��@P��@P�{@P�(@P� @P��@P��@P��@P�(@P��@P��@P��@P��@P�%@P��@P�@P�@P��@P��@P�.@P�@P��@P�@P�5@P��@P�@P��@P��@P�n@P�n@P�n@P��@P��@P��@P��@P��@P�B@P�@P��@P��@P��@P�@P��@P��@P��@P��@P�@P�@P��@P�@P��@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   44443444344344334444334434444444343444444444443344344444344344344444433434443443443344444344334444433443444434344444434444443344443443444334443344444433443334443344333344333334444434443333443333444433343433343343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�Ѷ9��9���9��9��t9��J9��9��.9�Я9���9���9�Ϗ9�ϒ9��9��9��i9���9���9��	9���9��
9�ν9���9���9��9���9��:9��:9�ώ9�Ϗ9��9��39��:9��M9��u9��9�҆9�҉9�Ҍ9�ѹ9��k9���9��+9�Ҭ9�ӕ9��9�Ѣ9���9��u9���9��E9�Ю9���9���9��"9�Ю9�Ь9�ϴ9��9��z9��D9��&9��Z9��9��Z9��U9�Ҭ9���9��S9��9�֍9��T9��q9���9���9��o9�Ֆ9���9�Э9��9��9��)9��9�ת9��.9��w9��<9���9���9���9��K9���9���9���9��
9�پ9�ٿ9�ل9��s9��9���9�ז9���9�ء9�؞9�؏9���9�؍9�؞9���9�؞9��g9���9��9���9���9���9���9���9��9��p9���9��h9��89���9���9��N9�ك9�٭9�م9��]9�پ9��09��n9�ع9���9��`9��)9��+9��;9�ٻ9�ٿ9���9��09��9���9���9���9��z9��19��&9�؏9���9��z9��9��9��H9��A9��T9��V9�ڥ9���9���9��9��9���9��h9��"9���9�ٰ9��o9��V9�ڶ9��9��k9�� 9��D9�ڸ9�ڐ9���9��:9��;9��M9�ݲ9���9���9��Y9���9���9��Y9���9���9=o�9=oa9=o�9=o�9=o�9=o�9=o^9=o�9=oZ9=o�9=o^9=o_9=nx9=n9=n9=m�9=m�9=m�9=m�9=n	9=nT9=n�9=n�9=o9=o�9=qs9=uR9=uP9=v�9=w�9=yW9=yw9=y09=xo9=w�9=v�9=v9=t�9=r�9=r09=p�9=n{9=k�9=j9=il9=hd9=f�9=e9=a�9=_Q9=]�9=]69=[�9=[r9=[K9=[&9=[)9=[N9=["9=[ 9=Z�9=Y�9=Y�9=X9=Wr9=V�9=U�9=UY9=T�9=T�9=Tv9=T*9=T#9=T9=T9=T9=T*9=T�9=T�9=T�9=T�9=U9=V�9=W�9=V�9=U�9=T�9=SG9=R89=R9=QS9=Q}9=P�9=Ps9=PL9=P(9=N�9=N�9=N9=N�9=N_9=N\9=N\9=N�9=N�9=N�9=N`9=N\9=N�9=N�9=N�9=N�9=N:9=N�9=N�9=N�9=N�9=N�9=N_9=N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B]/B\)B\)B]/B]/B[#B[#B[#B[#B[#B[#BZBZBZBYBYBZB}�B�1B��B��B�B�XB��B��BuBA�B<jB-B6FB7LB9XB:^B8RB5?BA�BB�B?}B<jB>wBD�BK�BO�BC�BD�BG�BJ�BS�BXBYBW
BR�BH�BF�BC�B@�BF�B;dB.B%�B �B�B�BuB�BJBB��B�B�5B�)B��BŢB�^B�3B��B��B�oB<jB"�B�BPB  B�B�#BÖB�B��B��B|�B_;B?}B"�B
=BB
�B
�;B
��B
��B
ÖB
�LB
��B
�\B
s�B
R�B
7LB
'�B
�B
{B
VB
B	��B	�`B	�;B	�
B	ĜB	��B	��B	�=B	v�B	D�B	,B	"�B	�B	�B	uB	VB		7B	B�B�sB�`B�NB�/B��B��BȴBŢBÖB�dB�?B�!B�B��B��B��B��B�oB�=B�%B�B�B�B�B�B|�Bw�Br�Bq�Bo�Bl�BiyBgmBbNBcTBcTBbNBcTBe`BdZBe`B`BBZBgmBhsBl�BhsBZBW
B\)BW
BO�BK�BL�BL�BK�BI�BL�BQ�BR�BVBVBW
BW
BXBZB\)B\)BaHBcTBffBiyBk�Bn�Bp�Br�Bt�Br�Bm�BjBo�Bs�Bv�Bw�Bv�Bu�Bq�Bq�Bo�Bo�Bk�Bk�BcTB[#BXBXBXBZB\)B`BBe`Be`BdZBaHBYBN�BL�BJ�BJ�BM�BP�BO�BS�BW
B[#B]/BiyBo�Bo�Bp�Bs�Bu�Bv�Bx�B|�B� B�B�B�B�B�B�B�+B�+B�B�B�%B�7B�JB�PB�\B�bB�oB�uB�uB�hB�hB�VB�bB�hB��B��B��B��B��B��B��B��B��B��B�B�FB�^B�dB�qB��B��BÖBƨB��B��B��B��B�
B�#B�/B�/B�/B�/B�)B�#B�/B�NB�ZB�`B�fB�sB�B�B��B��B��B��B	B	B	B	%B		7B	PB	hB	�B	�B	�B	�B	�B	!�B	#�B	'�B	,B	2-B	6FB	<jB	=qB	A�B	G�B	K�B	M�B	O�B	Q�B	S�B	XB	\)B	_;B	cTB	cTB	cTB	dZB	hsB	k�B	l�B	m�B	n�B	q�B	s�B	s�B	q�B	s�B	w�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�PB	�\B	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�3B	�?B	�RB	�XB	�^B	�dB	�qB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
%B
�B
kB
!�B
)DB
0!B
5ZB
<PB
D3B
J=B
QhB
U�B
[=B
`\B
d�B
iDB
lB
n�B
sMB
x8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Af��?�4z>�(_?�ȊB-
?�0?7�Au�
Bk�?R*@�֝B=�>�<?*F(A��XB'�?͘|@�i�?um?�/�B�Bs|?6r?�
A�>ɹ�?va�>�`�>Ě�?_=?A�UA�t�Bz�?Dk{A�Z�>��X>ꃲ>�4�?;�?6��?BQ�A!��?.x�?8)(@
su?D��B4vB�@��??r.MA���An?��@R�?�?��BZZ?b@<\Bc?%�o@�^WB(Z?�9>���?��?�.�? ٥?�I�B" B;�?-�gA��h@~��?q�?\0�B$@z_-?�סB,�?{�?���B@B,?�I�@ԷA��@��m?O��A��R?*5?cd�B>�A��E>�1>��@�h+@YR�??�A�cvB	E�?��A�Bԝ?F��>�_?��@j0QB?�?��FB)>�+�>���>�8�>���?!�t@p�Bq?�AL�f?��?���?��?���B$bB��>�5?&@��A�B+�A(�?�B-x>�?˴?��B&�BPN>ƫ?
�_?�[B(QB�_?,8�@��@���@���A']?��B'�B*GAf��?.*B ��A��A�� ?/<?\BA[��B$5B1@5�%?n� B,_B'�B&	B-(?�h?�.B��B)-B(�B&mB2?�@��@���A!lq@S
�B>�>��2?/?t�]B(SB*B)SB5?ѷ?�sKA��B�QB*�B2V?C,�@�`A�c@�oB*�B-B"�@�ѯA�y?H|BB�B(�B-@�OB�B $AP�B+�B3SB-�B.�@D��A�zB,6B*�B*�A�B\/B+�B+B,(B+�B+�B+�B*�B+�B)�B++B,�B-�B+�B+oB+�B*BB-B+�B,�B+�B+�B+B-B,�B*�B.B+1B+�B/aB/�B+]B+B+�B+�B
��B+AB.mB.�B,4B+�B-�B,hB)dB.�B,RB+^B*�B+UB,fB(�B(�B+{B-�B*aB,B*�B+�B*�B,�B+�B,B-�B+�B+�B+�B,�B+$B*�B+�B+�B*uB,B/jB/�B-B+B*�B,�B,!B,EB+�B,�B*�B,B/B*�B*�B*�B+�B*)B+rB+,B-B,�B,B-}B,`B+B.(B+{B*uB0�B*�B+MB-�B,[B,�B-BB,�B,�B,�B+�B-\B-GB+UB*�B,�B.B+iB*�B-�B,=B,B,�B+�B*�B+�B0B+[B*�B)�B6B-B,B*"B,0B.B,}B0tB,�B-VB- B+�B*�B/vB*B)�B+�B+�B-B)�B,�B,B*�B.6B+B+�B*�B(MB*DB*uB*jB*�B(�B(7B*�B,�B*�B+zB)�B)]B)�B+�B,�B+�B*�B)�B+$B*�B*�B+eB-B.B+�B+CB*�B,B*RB*�B+�B*B*�B-9B+%B,�B*�B-�B*�B,B+�B+B)�B,)B*�B*�B+WB(�B)�B+�B,�B+�B+�B+NB-VB+`B,[B.B.�B.>B-NB)�B*�B+:B-kB,RB+WB.�B*�B+�B.�BBIB*�B*�B-�B+�B)0B+�B,*B+CB+�B*�B,9B-�B.YB)@B0IB-uB+�B+YB*B*B+5B+�B)�B+oB+�B+�B*�B*�B*�B)sB(OB+BB*�B*xB+�B+aB*oB*B+�B-[B+ZB)�B*�B(_B'~B+ B+�B,B+�B,=B,�B- B-�B/+B.�B4[B-.B+�B,�B/�B+�B*�B)�B*�B*�B*�B)�B*�B*2B)�B**B)�B*5B*-B+�B*�B+�B*;B*B)�B*xB*6B*�B+GB*|B+eB*kB*cB*�B*�B+<B+�B+�B+�B,�B+�B-�B*�B)�B+zB*�B*�B*�B,B*�B+4B*�B*�B+B+|B+tB+�B*.B*kB)JB+9B*�B*�B*-B+xB+�B,mB+�B+�B+�B+�B,jB+�B*�B+jB+�B*�B,B+�B,fB+�B*VB*�B+-B,�B,B,pB+�B+�B*(B)�B)B,�B-�B-<B,�B/�B-�B);B'B-�B.�B,vB.gB*�B,B+�B+:B+1B+)B*�B)�B+�B+B+�B+�B,WB,�B,*B,B+FB+5B+{B+B+�B+�B+IB+8B+�B+�B+�B*�B+�B+NB+�B+B*�B+�B+B+yB,B+�B*�B+B+�B*�B*�B*%B*9B*�B+0B*]B,CB+�B*�B+�B+�B+gB+VB+ B+>B+�B+%B*�B+�B+yB,3B,B+B+B+6B+sB+bB+ZB,�B,=B+bB,$B*�B*OB*�B*�B+�B+6B*+B+�B+oB,B+�B+�B*�B,aB,�B,@B+<B+�B-UB,�B+<B-"B-TB,>B-OB->B-�B--B-�B	ɠB	��B	ʵB	��B	��B	ʒB	�fB	ʖB	�OB	�1B	��B	ʟB	�B	�+B	��B	�B	�B	�B	�JB	�\B	˝B	��B	�B	̝B	��B	̃B	� B	�1B	˾B	�:B	�gB	�;B	ɇB	��B	��B	�B	�B	ȜB	��B	�LB	�UB	�4B	��B	�AB	�mB	ȸB	ƱB	ǪB	�B	�]B	��B	��B	�%B	��B	ʂB	�VB	�hB	�B	��B	�=B	��B	ɋB	�lB	��B	�_B	��B	ʎB	�UB	��B	ʣB	ʵB	˸B	�B	�SB	�FB	�iB	�\B	ʍB	�SB	ˢB	ˈB	�4B	ʾB	�
B	�B	�FB	�qB	�B	�VB	��B	�B	�xB	�|B	�aB	��B	�aB	�	B	�B	̤B	ͧB	͚B	ͽB	ͰB	ͣB	͈B	�NB	�nB	�B	�	B	�jB	�\B	��B	�B	��B	�+B	�B	��B	��B	͞B	��B]B]+B\�B]�B^B\�B]uB]B\|B]nB]RB]�B]�B]B]%B]jB]�B\�B\�B]�B]�B]UB]|B]`B]�B]B]wB]wB]-B]�B]B]1B]eB\�B]�B]-B\�B]�B]�B]vB] B]�B]B]}B]�B]�B] B]oB]B]VB]`B]�B]�B]�B]B]_B]VB]�B]bB^iB]RB]pB]UB^B^B^B]pB\�B]�B]�B]GB\�B\�B]/B]�B[�B^sB^%B^%B]�B]�B]�B]�B]*B]�B^&B]�B]`B\�B\rB]�B]VB]�B]vB]�B]6B].B]�B]�B])B]�B]�B]|B^"B^B]�B^B]B]�B^B]�B]�B]	B];B]�B\�B\�B]�B\�B\�B\�B\�B\�B\�B\~B\�B\�B]B]!B\�B\�B\@B]oB\�B[�B[�B\hB\B\�B\�B\�B]QB\�B\�B\�B\rB]-B]B\�B]QB]�B]$B\�B\�B\aB\�B]	B\�B]B\�B]8B]�B]FB]pB\B\ B\�B\UB[�BZ�B[�B\TB[�B\cB\�B\^B[3B\kB\3B[�B]>B]6B]9B\�B]B]�B]KB]�B\�B]*B]�B]hB	�4B	��B	�B	��B	�B	��B	�B	�B	�uB	�B	�NB	�QB	�~B	�%B	�B	�B	�B	�B	�B	�B	��B	�B	�2B	�DB	�B	��B	��B	��B	��B	�B	�B	�B	�bB	�B	�6B	�CB	��B	�B	��B	�B	�ZB	�B	�}B	�B	�B	�B	�gB	�
B	�jB	�sB	�B	��B	�B	�SB	�B	��B	��B	��B	�B	�B	�5B	�B	�9B	�B	�rB	��B	�B	�B	�FB	��B	��B	�B	�mB	�RB	�EB	�+B	�<B	�~B	�B	�B	�vB	�B	�B	�zB	��B	��B	�B	��B	��B	��B	�;B	�PB	��B	�QB	�6B	�
B	�	B	��B	�:B	�B	�]B	�BB	�5B	�GB	�jB	�>B	��B	��B	��B	��B	��B	�B	�nB	��B	�B	�B	�YB	�nB	�$B	�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944443444344344334444334434444444343444444444443344344444344344344444433434443443443344444344334444433443444434344444434444443344443443444334443344444433443334443344333344333334444434443333443333444433343433343343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B]B]B]B]B]B]B]B]B]B]B]B\B\B] B]B[B[B[B[B[B[BZBZ
BZ
BYBYBZB}�B� B��B��B�B�FB̼B��BbBAuB<XB-B65B7:B9EB:MB8=B5.BAxBBB?nB<YB>dBD�BK�BO�BC�BD�BG�BJ�BS�BW�BYBV�BR�BH�BF�BC�B@pBF�B;SB-�B%�B �B�B}BaB�B7BB��B�B�$B�B��BŒB�MB�B��B��B�^B<VB"�B�B=B��B�B�BÄB�B��B��B|�B_%B?kB"�B
(B�B
�~B
�(B
��B
˴B
ÀB
�;B
��B
�JB
s�B
R�B
78B
'�B
�B
hB
?B
B	��B	�KB	�)B	��B	ĈB	��B	��B	�)B	v�B	D�B	+�B	"�B	�B	rB	_B	CB		"B	 �B�B�^B�JB�9B�B��BʬBȟBōBÁB�OB�)B�B��B��B��B��B�xB�XB�'B�B�	B�	B�B�B��B|�Bw�Br�Bq�Bo�BluBieBgUBb5Bc=Bc<Bb6Bc?BeJBdABeHB`*BZBgVBh_BlsBhZBZBV�B\BV�BO�BK�BL�BL�BK�BI�BL�BQ�BR�BU�BU�BV�BV�BW�BZB\B\Ba0Bc;BfOBiaBkoBn�Bp�Br�Bt�Br�BmxBjgBo�Bs�Bv�Bw�Bv�Bu�Bq�Bq�Bo�Bo�BkjBkmBc;B[BW�BW�BW�BZB\B`)BeIBeHBdABa.BY BN�BL�BJ�BJ�BM�BP�BO�BS�BV�B[
B]BiaBo�Bo�Bp�Bs�Bu�Bv�Bx�B|�B�B��B��B��B��B��B��B�B�B��B��B�B�B�0B�7B�AB�HB�UB�[B�\B�NB�NB�;B�IB�OB�hB�iB��B��B��B��B��B��B��B��B��B�*B�DB�IB�ZB�iB�iB�}BƎB˫B̳B��B��B��B�B�B�B�B�B�B�B�B�6B�@B�DB�MB�XB�~B�B��B��B��B��B	 �B	�B	B	
B		B	7B	PB	sB	rB	yB	�B	�B	!�B	#�B	'�B	+�B	2B	6+B	<QB	=ZB	AnB	G�B	K�B	M�B	O�B	Q�B	S�B	W�B	\B	_B	c8B	c9B	c8B	dAB	h\B	kjB	lpB	myB	n~B	q�B	s�B	s�B	q�B	s�B	w�B	|�B	~�B	�B	��B	��B	��B	��B	��B	�
B	�B	�#B	�6B	�BB	�FB	�ZB	�`B	�aB	�fB	�yB	��B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�7B	�<B	�CB	�JB	�VB	�dB	�hB	�mB	�tB	�B	�B	ňB	ŇB	ƏB	ǔB	ȘB	ˮB	ˬB	ˬB	˪B	̰B	ͷB	νB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�'B	�-B	�3B	�@B	�EB	�DB	�LB	�SB	�WB	�`B	�eB	�eB	�gB	�fB	�jB	�qB	�uB	�}B	�|B	�B	�B	�}B	�~B	��B	��G�O�B	��B
B
�B
PB
!�B
),B
0B
5?B
<6B
DB
J#B
QNB
UjB
["B
`AB
d�B
i+B
k�B
n�B
s3B
xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B,�G�O�G�O�G�O�Bk�G�O�G�O�B=�G�O�G�O�A��EB'�G�O�G�O�G�O�G�O�B�BsfG�O�G�O�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz{G�O�A�Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B4gB�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�BZNG�O�G�O�Bb�G�O�G�O�B(LG�O�G�O�G�O�G�O�G�O�G�O�B"B;�G�O�A��OG�O�G�O�G�O�B#�G�O�G�O�B,�G�O�G�O�B0B, G�O�G�O�G�O�G�O�G�O�A��8G�O�G�O�B>�A��0G�O�G�O�G�O�G�O�G�O�A�cWB	E�G�O�G�O�BԍG�O�G�O�G�O�G�O�B?�G�O�B)G�O�G�O�G�O�G�O�G�O�G�O�BqG�O�G�O�G�O�G�O�G�O�G�O�B$SB��G�O�G�O�G�O�G�O�B+�G�O�G�O�B-eG�O�G�O�G�O�B&zBP<G�O�G�O�G�O�B(@B�LG�O�G�O�G�O�G�O�G�O�G�O�B'�B*6G�O�G�O�B ��A��A��	G�O�G�O�G�O�B$'B1G�O�G�O�B,OB'�B%�B-G�O�G�O�B��B)B(�B&`B1�G�O�G�O�G�O�G�O�G�O�B>�G�O�G�O�G�O�B(@B*B)BB4�G�O�G�O�A퀮B�?B*rB2DG�O�G�O�G�O�G�O�B*�B-B"�G�O�A�^G�O�BqB(�B,�G�O�B�B G�O�B+�B3BB-�B.�G�O�A�^B,!B*�B*�A��B\B+�B+ B,B+~B+�B+�B*�B+rB)�B+B,�B-�B+�B+aB+yB*/B-mB+�B,�B+�B+�B+B-B,�B*�B. B+ B+�B/RB/�B+LB+ B+~B+�B
��B+.B._B.�B,&B+�B-B,YB)RB.�B,CB+MB*�B+CB,VB(�B(�B+kB-�B*LB,B*wB+�B*�B,�B+�B+�B-�B+�B+yB+�B,�B+B*�B+{B+�B*`B,B/[B/�B,�B*�B*�B,�B,B,2B+�B,�B*�B,B/B*�B*�B*�B+�B*B+aB+B,�B,�B+�B-mB,OB+B.B+kB*`B0�B*�B+<B-|B,KB,yB-2B,�B,�B,�B+�B-MB-6B+CB*�B,rB-�B+ZB*�B-�B,+B,B,�B+�B*�B+�B/�B+IB*�B)yB'B-B, B*B,!B-�B,mB0aB,yB-HB,�B+�B*�B/iB)�B)�B+rB+�B,�B)�B,�B,B*�B.&B+B+uB*�B(;B*1B*`B*YB*�B(�B(#B*�B,�B*�B+gB)�B)JB)�B+�B,�B+�B*�B)�B+B*�B*�B+RB,�B-�B+uB+1B*�B,B*AB*�B+{B)�B*�B-(B+B,�B*�B-�B*�B,B+�B*�B)�B,B*rB*rB+EB(�B){B+�B,�B+�B+�B+>B-HB+RB,KB.B.�B.,B-=B)�B*�B+%B-\B,CB+EB.�B*�B+�B.�BB5B*�B*�B-�B+{B)B+�B,B+.B+wB*�B,(B-�B.KB)2B0:B-fB+uB+FB* B* B+%B+�B)�B+\B+�B+�B*�B*�B*�B)aB(@B+3B*�B*eB+yB+OB*^B)�B+�B-MB+HB)�B*�B(OB'lB+B+�B,B+�B,+B,�B,�B-|B/B.�B4JB-B+�B,�B/�B+�B*tB)�B*�B*�B*�B)�B*�B* B)�B\�B]B\�B]�B]�B\�B]cB]B\nB]\B]AB]�B]�B]B]B]UB]�B\�B\�B]�B]�B]AB]iB]OB]�B\�B]gB]gB]B]�B]B] B]SB\�B]�B]B\�B]�B]{B]gB]B]sB\�B]lB]�B]�B]B]_B]B]DB]OB]wB]�B]�B]B]OB]DB]�B]QB^UB]AB]\B]HB]�B^B]�B]\B\�B]�B]�B]6B\B\�B]B]�B[�B^cB^B^B]�B]�B]�B]�B]B]�B^B]�B]OB\�B\bB]�B]AB]�B]gB]�B]$B]B]�B]�B]B]�B]�B]lB^B]�B]�B^B\�B]�B^ B]�B]pB\�B])B]�B\�B\�B]�B\�B\�B\uB\�B\�B\�B\lB\�B\�B\�B]B\�B\�B\.B]\B\�B[�B[�B\XB\B\�B\�B\{B]?B\�B\�B\�B\_B]B]B\�B]?B]�B]B\�B\�B\QB\�B\�B\�B\�B\�B]'B]uB]2B]_B[�B\B\�B\GB[�BZ�B[�B\BB[�B\VB\�B\NB[!B\ZB\"B[�B]+B]$B]$B\�B\�B]�B];B]�B\�B]B]~B]UB	�B	��B	��B	��B	��B	��B	�B	�B	�XB	�lB	�2B	�8B	�cB	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�+B	�|B	��B	�B	�B	�B	�B	��B	�B	�HB	�B	�B	�(B	�B	�B	��B	�nB	�?B	�B	�bB	��B	�oB	�B	�NB	��B	�MB	�YB	��B	�B	�tB	�:B	� B	��B	��B	��B	�B	�sB	�B	�B	� B	��B	�WB	��B	��B	�B	�+B	��B	�B	�oB	�PB	�8B	�*B	�B	�"B	�dB	�uB	�xB	�[B	�B	�B	�`B	�B	��B	��B	��B	��B	��B	� B	�6B	�B	�8B	�B	��B	��B	��B	�B	�pB	�DB	�&B	�B	�,B	�OB	�!B	��B	�B	��B	��B	�B	�B	�VB	�sB	��B	�{B	�?B	�VB	�B	�:B\�B]B\�B]�B]�B\�B]cB]B\nB]\B]AB]�B]�B]B]B]UB]�B\�B\�B]�B]�B]AB]iB]OB]�B\�B]gB]gB]B]�B]B] B]SB\�B]�B]B\�B]�B]{B]gB]B]sB\�B]lB]�B]�B]B]_B]B]DB]OB]wB]�B]�B]B]OB]DB]�B]QB^UB]AB]\B]HB]�B^B]�B]\B\�B]�B]�B]6B\B\�B]B]�B[�B^cB^B^B]�B]�B]�B]�B]B]�B^B]�B]OB\�B\bB]�B]AB]�B]gB]�B]$B]B]�B]�B]B]�B]�B]lB^B]�B]�B^B\�B]�B^ B]�B]pB\�B])B]�B\�B\�B]�B\�B\�B\uB\�B\�B\�B\lB\�B\�B\�B]B\�B\�B\.B]\B\�B[�B[�B\XB\B\�B\�B\{B]?B\�B\�B\�B\_B]B]B\�B]?B]�B]B\�B\�B\QB\�B\�B\�B\�B\�B]'B]uB]2B]_B[�B\B\�B\GB[�BZ�B[�B\BB[�B\VB\�B\NB[!B\ZB\"B[�B]+B]$B]$B\�B\�B]�B];B]�B\�B]B]~B]UB	�B	��B	��B	��B	��B	��B	�B	�B	�XB	�lB	�2B	�8B	�cB	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�+B	�|B	��B	�B	�B	�B	�B	��B	�B	�HB	�B	�B	�(B	�B	�B	��B	�nB	�?B	�B	�bB	��B	�oB	�B	�NB	��B	�MB	�YB	��B	�B	�tB	�:B	� B	��B	��B	��B	�B	�sB	�B	�B	� B	��B	�WB	��B	��B	�B	�+B	��B	�B	�oB	�PB	�8B	�*B	�B	�"B	�dB	�uB	�xB	�[B	�B	�B	�`B	�B	��B	��B	��B	��B	��B	� B	�6B	�B	�8B	�B	��B	��B	��B	�B	�pB	�DB	�&B	�B	�,B	�OB	�!B	��B	�B	��B	��B	�B	�B	�VB	�sB	��B	�{B	�?B	�VB	�B	�:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944443444344344334444334434444444343444444444443344344444344344344444433434443443443344444344334444433443444434344444434444443344443443444334443344444433443334443344333344333334444434443333443333444433343433343343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649242020083116492420200831164924202008311649242020083116492420200831164924202008311649242020083116492420200831164924202008311649242020083116492420200831164924AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817152019021918171520190219181715    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817152019021918171520190219181715  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817152019021918171520190219181715  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649242020083116492420200831164924  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                