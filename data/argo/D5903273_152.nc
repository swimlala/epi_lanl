CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  J   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:14Z creation      
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
resolution        =���   axis      Z        'x  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'x  vh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� (   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x 1�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'x Y`   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'x ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �0   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'x �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�     PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� <X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'x F8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � m�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   np   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   zp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181714  20200831164923  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�Q��5+�@�Q��5+�@�Q��5+�111 @�Q�[��@�Q�[��@�Q�[��@5��O�;d@5��O�;d@5��O�;d�cpbM���cpbM���cpbM��111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BR  BV  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� DtfDt�fDt�fDyy�D�\D�R�D�yHD�ʏD�RD�O\D��RD�ÅD��D�ND���Dǿ
D��D�?
Dڇ\D� D��D�:=D�{�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;������������L�;L�;������������L��    ���;������;L�;L�;����������������L�;���������������    >L�ͽ��;����L�;������;����L�;����L�;������������������������L�;��������������������������������L��    ���������������;L�;��������L��    ���������L�;����������������L��    ���;��������L�;�����������    ���;�������������������    �L�;������;L�;��������L�;����L�ͽ��;�������    >L��=��;��������L�;L�ͽ��ͽ��;����L�;L�;����L�ͽ��;L�;��������L�;L�;������������������������L�;�����������    ���;��������L�;L�;L�;������;������������L�;����L�ͽ���    ���������L��=���    ���;��������������;��������L�ͽ��;������;L�;����L�;L�;L�;L�;����L�;L�;����L�ͽ��;L�;����L�;�������        �����L�;L�;L�ͽ���=��ͽ��;��������L�;L�ͽ��ͽ��;L�;�������            ���;������ͽ��ͽ��;L�;L�;L�;L�;L�;����L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    =��ͽ��ͽ���            ����    =���    ����=��ͽ��ͽ���    >L��        ����    ���ͽ���=��ͽ���    =���    �L��=���    ����>L�ͽ��ͽ��ͽ���    ����    =���            =���    ���ͽ���        ����            �L��    ����            =���=���            =���=���    ����            ���ͽ��ͽ���    ���;L��                        =���    =���=���    ���ͽ��ͽ���=���    ����=��ͽ���=���    ����                    ���ͽ���                        ����=���=���        ���ͽ��ͽ��ͽ���            �L��                �L�ͽ���    ���ͽ���    ����                ���ͽ���    ����        ����    �L��        ����                ����=���    ���ͽ���=���    ����=���        ����            =���    ����            ���ͽ���        ����                =���            ����                        ����    ���ͽ���            ����                ����    =���=���    >L��=��ͽ��ͽ���    >L��=��ͽ���            =���=���>L��=���=���>L��>L��>���>���>L��>���>���>���>���>���?   >���>���>���?   ?   ?��?   ?333?��?��?333?L��?fff?fff?L��?fff?�  ?�  ?�  ?���?���?���?���?�ff?�ff?�ff?�33?�  ?�  ?���?���?ٙ�?ٙ�?ٙ�?ٙ�?�ff?�33?�33?�33?�33@   @ff@ff@��@ff@��@33@33@��@��@��@   @   @   @&ff@,��@,��@,��@333@9��@9��@@  @Fff@L��@L��@S33@S33@`  @`  @fff@fff@l��@s33@y��@y��@y��@�  @�33@�ff@�ff@���@���@���@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@���@�  @�33@�ff@���@�  @�33@�33@�ff@���@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33A��A  A	��A33AffA  A��A��AffA  A33A��A   A!��A#33A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�  A���A͙�A�ffA�33A���Aљ�A�ffA�33A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A���Dq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3DsٚDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��Du  @,��@,��@333@9��@9��@@  @Fff@L��@L��@S33@S33@`  @`  @fff@fff@l��@s33@y��@y��@y��@�  @�33@�ff@�ff@���@���@���@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@���@�  @�33@�ff@���@�  @�33@�33@�ff@���@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33A��A  A	��A33AffA  A��A��AffA  A33A��A   A!��A#33A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�  A���A͙�A�ffA�33A���Aљ�A�ffA�33A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A���Dq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3DsٚDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @B�\@��@�z�A�
A#�
AC�
Ac�
A��A��A��A��A��A��A�RA��B ��B��B��B��B ��B)\)B0��B8��B@��BH��BR��BV��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq�Dq�\Dr\Dr�\Ds\Ds�\Dt�Dt��Dt��Dy��D�
D�Z�D���D��=D� D�W
D�� D��3D��D�U�D��\D�ƸD��D�F�Dڏ
D࿮D���D�A�D�D��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>\)�u�u�u=#�	=#�	�u�u�u=#�	>u>\)�u>\)=#�	=#�	�u�u�u�u=#�	�u�u�u�u>u>�G�>\)�u=#�	�u>\)�u=#�	�u=#�	�u�u�u�u�u�u=#�	�u�u�u�u�u�u�u�u=#�	>u�u�u�u>\)=#�	�u�u=#�	>u�u�u=#�	�u�u�u�u=#�	>u>\)�u�u=#�	�u�u>\)>u>\)�u�u�u�u�u>u=#�	�u>\)=#�	�u�u=#�	�u=#�	>\)�u�u>u>�G�>�{�u�u=#�	=#�	>\)>\)�u=#�	=#�	�u=#�	>\)=#�	�u�u=#�	=#�	�u�u�u�u�u�u=#�	�u�u>\)>u>\)�u�u=#�	=#�	=#�	�u>\)�u�u�u=#�	�u=#�	>\)>u�u�u=#�	>�{>u>\)�u�u�u>\)�u�u=#�	>\)�u>\)=#�	�u=#�	=#�	=#�	=#�	�u=#�	=#�	�u=#�	>\)=#�	�u=#�	�u>\)>u>u�u=#�	=#�	=#�	>\)>�{>\)�u�u=#�	=#�	>\)>\)=#�	�u>\)>u>u>u>\)�u>\)>\)>\)=#�	=#�	=#�	=#�	=#�	�u=#�	=#�	>\)>\)>\)>\)>\)>\)>u>�{>\)>\)>u>u>u>\)>u>�{>u>\)>�{>\)>\)>u>�G�>u>u>\)>u>\)>\)>�{>\)>u>�{>u=#�	>�{>u>\)>�G�>\)>\)>\)>u>\)>u>�{>u>u>u>�{>u>\)>\)>u>u>\)>u>u>u=#�	>u>\)>u>u>u>�{>�{>u>u>u>�{>�{>u>\)>u>u>u>\)>\)>\)>u>\)=#�	>u>u>u>u>u>u>�{>u>�{>�{>u>\)>\)>\)>�{>u>\)>�{>\)>�{>u>\)>u>u>u>u>u>\)>\)>u>u>u>u>u>u>\)>�{>�{>u>u>\)>\)>\)>\)>u>u>u=#�	>u>u>u>u=#�	>\)>u>\)>\)>u>\)>u>u>u>u>\)>\)>u>\)>u>u>\)>u=#�	>u>u>\)>u>u>u>u>\)>�{>u>\)>\)>�{>u>\)>�{>u>u>\)>u>u>u>�{>u>\)>u>u>u>\)>\)>u>u>\)>u>u>u>u>�{>u>u>u>\)>u>u>u>u>u>u>\)>u>\)>\)>u>u>u>\)>u>u>u>u>\)>u>�{>�{>u>�G�>�{>\)>\)>u>�G�>�{>\)>u>u>u>�{>�{>�G�>�{>�{>�G�>�G�?
=q?
=q>�G�?
=q?
=q?#�
?#�
?#�
?=p�?#�
?#�
?#�
?=p�?=p�?W
>?=p�?p��?W
>?W
>?p��?��?��?��?��?��?��R?��R?��R?��?��?�Q�?�Q�?��?��?��?��?޸R?޸R?�?�?�Q�?�Q�?�Q�?�Q�@�\@��@��@��@��@\)@@@(�@@(�@"�\@"�\@(��@(��@(��@/\)@/\)@/\)@5@<(�@<(�@<(�@B�\@H��@H��@O\)@U@\(�@\(�@b�\@b�\@o\)@o\)@u@u@|(�@�G�@�z�@�z�@�z�@��@��G@�z@�z@�G�@�z�@�z�@��@��G@�z@�z@�G�@�z�@��@��G@�z@�G�@�G�@�z�@��@��G@�z@�G�@Ǯ@��G@��G@�z@�z�@׮@��G@�z@�G�@�@��G@�z@�G�@��@��G@�zA ��A�
Ap�A
=A��A�
Ap�A
=A=pA�
Ap�A��A=pA�
A
=A ��A#�
A%p�A'
=A*=pA+�
A-p�A0��A2=pA3�
A5p�A8��A:=pA;�
A?
=A@��AB=pAEp�AG
=AH��AK�
AMp�AO
=AP��AS�
AUp�AW
=AZ=pA[�
A]p�A`��Ab=pAep�Ag
=Aj=pAk�
Ao
=Ap��Ar=pAup�Aw
=Az=pA{�
A
=A�Q�A��A��RA��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A¸RA�Q�A��A��AǅA�Q�A��A��A˅A�Q�A��AθRAυA�Q�A��AҸRAӅA�Q�A��AָRAׅA�Q�A��A��AۅA�Q�A��A޸RDq)Dq"�Dq/\Dq5�DqB�DqH�DqO\Dq\)Dqb�Dqh�Dqu�Dq|)Dq��Dq�\Dq��Dq�)Dq��Dq�\Dq�)DqDq��Dq�\Dq�)Dq�Dq��Dq��Dq�)Dr�Dr\Dr�Dr)Dr(�Dr/\Dr5�DrB�DrH�DrO\Dr\)Drb�Drh�Dru�Dr|)Dr��Dr�\Dr��Dr�)Dr��Dr�\Dr��DrDr��Dr�\Dr�)Dr�Dr��Dr��Dr�)Ds�Ds�Ds�Ds)Ds(�Ds/\Ds5�DsB�DsH�DsO\DsU�Dsb�Dsh�Dso\Ds|)Ds��Ds��Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�Ds��Ds�\Ds�)Dt�Dt�Dt�Dt)Dt"�Dt/\Dt5�Dt<)DtB�DtO\DtU�Dtb�Dth�Dto\Dtu�Dt��Dt��Dt��Dt�)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��Dt�Dt��Dt�\Dt�)Du�Du�Du\@<(�@<(�@B�\@H��@H��@O\)@U@\(�@\(�@b�\@b�\@o\)@o\)@u@u@|(�@�G�@�z�@�z�@�z�@��@��G@�z@�z@�G�@�z�@�z�@��@��G@�z@�z@�G�@�z�@��@��G@�z@�G�@�G�@�z�@��@��G@�z@�G�@Ǯ@��G@��G@�z@�z�@׮@��G@�z@�G�@�@��G@�z@�G�@��@��G@�zA ��A�
Ap�A
=A��A�
Ap�A
=A=pA�
Ap�A��A=pA�
A
=A ��A#�
A%p�A'
=A*=pA+�
A-p�A0��A2=pA3�
A5p�A8��A:=pA;�
A?
=A@��AB=pAEp�AG
=AH��AK�
AMp�AO
=AP��AS�
AUp�AW
=AZ=pA[�
A]p�A`��Ab=pAep�Ag
=Aj=pAk�
Ao
=Ap��Ar=pAup�Aw
=Az=pA{�
A
=A�Q�A��A��RA��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A¸RA�Q�A��A��AǅA�Q�A��A��A˅A�Q�A��AθRAυA�Q�A��AҸRAӅA�Q�A��AָRAׅA�Q�A��A��AۅA�Q�A��A޸RDq)Dq"�Dq/\Dq5�DqB�DqH�DqO\Dq\)Dqb�Dqh�Dqu�Dq|)Dq��Dq�\Dq��Dq�)Dq��Dq�\Dq�)DqDq��Dq�\Dq�)Dq�Dq��Dq��Dq�)Dr�Dr\Dr�Dr)Dr(�Dr/\Dr5�DrB�DrH�DrO\Dr\)Drb�Drh�Dru�Dr|)Dr��Dr�\Dr��Dr�)Dr��Dr�\Dr��DrDr��Dr�\Dr�)Dr�Dr��Dr��Dr�)Ds�Ds�Ds�Ds)Ds(�Ds/\Ds5�DsB�DsH�DsO\DsU�Dsb�Dsh�Dso\Ds|)Ds��Ds��Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�Ds��Ds�\Ds�)Dt�Dt�Dt�Dt)Dt"�Dt/\Dt5�Dt<)DtB�DtO\DtU�Dtb�Dth�Dto\Dtu�Dt��Dt��Dt��Dt�)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��Dt�Dt��Dt�\Dt�)Du�Du�Du\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�"�A�&�A�1'A�/A�33A�1'A�33A�9XA�9XA�;dA�;dA�=qA�=qA�=qA�=qA�;dA�;dA�33A�/A�1'A�-A�(�A��A���A���A�VA�t�A���A���A���A�ƨA�ƨA�t�A�K�A�1'A��FA��
A�I�A��A���A���A�33A���A���A�v�A�n�A�%A���A��uA�M�A�VA�ĜA���A��A�jA�{A���A���A���A���A���A��hA�r�A�7LA�"�A��A��A��A�XA� �A��A���A��A���A���A�x�A�"�A�%A���A��uA�r�A�^5A�^5A�Q�A�M�A�E�A���A���A�bNA�JA�\)A��A�ffA��A��A�XA���A�\)A��A��uA��uA�A���A��FA�A�A��\A��A���A�M�A���A�  A���A�(�A� �A���A���A�bA��jA�XA�~�A��!A�;dA�`BA~~�A|1'AxZAu��Am�;Aj��Ag��AdM�Ab��A^ȴA\�`A[�
A[�AY�^AWG�AT�AS�TAS?}AQ��AQ%AO�mAOVAM;dAKK�AI`BAGAF�9AF�DAEp�AD��AC��AB��A@��A?�A>ȴA=\)A<5?A;`BA8�A65?A3�A2��A0��A/O�A-�^A*�A(~�A'�A&�`A&{A%��A$ȴA$1A#"�A ffA�TAhsA��A��Az�A��AXA�A?}AA�A\)A��AZA�-A�`A��A��A��AC�AM�AhsAx�A\)A7LA�/A�A�A �A�TAdZA�-A	`BA+A�`A��AffAS�AĜA=qAt�A�FA =q@��+@��7@�%@���@���@��@�J@���@�{@��@��T@�9X@�F@�ȴ@�$�@�X@�C�@��@�Z@���@�?}@�|�@�"�@���@�V@ؼj@�Z@և+@��@Ͼw@�J@�`B@��@̛�@��@��@���@˥�@�K�@ʟ�@�?}@ț�@�1'@��@�@ēu@�9X@�t�@�X@�b@�dZ@��T@��@�K�@��@��!@��7@��/@�Q�@�(�@�1@���@��@��m@��@�l�@�+@�~�@�J@�G�@�&�@�V@���@�|�@���@�^5@�7L@�r�@�I�@��@�;d@�-@�@�J@�-@�=q@��@���@�%@�Q�@�t�@�ȴ@��@��-@��`@��w@�t�@�+@�"�@��H@���@�@��h@�`B@��@���@��@���@�z�@�9X@�(�@��m@�K�@�33@��H@���@�X@��j@�I�@�1@�|�@��y@�n�@�-@��7@�V@��u@�Z@��@���@�G�@���@��9@�z�@���@���@���@��;@��;@��@��@���@���@�Ĝ@���@�33@�@��y@�l�@��\@��T@��h@��@�O�@�/@�&�@���@���@�Ĝ@�Ĝ@��@��D@��F@���@�dZ@�C�@�
=@��y@��y@�+@�33@�dZ@��y@�ff@��R@���@�~�@�5?@�{@���@��T@��-@�p�@�`B@�O�@��/@��@��;@�  @�  @���@�ff@�=q@�{@��@�$�@�{@��@���@���@�p�@��7@�x�@�`B@�X@�&�@���@���@���@�Z@� �@�1@��@��;@���@�dZ@�\)@�+@��H@��!@���@���@�n�@�^5@�E�@�5?@�-@�J@���@���@���@�@�@���@�X@�7L@��`@��D@�1'@��F@�l�@�C�@�33@�
=@��H@���@��h@�hs@�`B@�`B@�&�@�%@���@��`@��j@��u@�Z@�(�@��@��@�33@�"�@�"�@��@�@���@��+@�~�@�U�@z@o9�@d�@\Z@U�X@N;�@F^5@@A�@;+@2��@.�B@*v�@%<6@ e�@)_@�@��@i�@
p;@dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A�  A��A��uA�E�A�oA��A��A��A�A���A�7LAăA�  AƶFA�K�A���A��A�$�A��Aŉ7A���A���A�XA��`A���A��A�jA��mAÍPAŲ-A�+A�E�A�JA�l�A�33A��hA�bA�Q�A�t�A�VA��A�ZA���A���A�?}A�bA�7LA��A��A���A���A���A���A���A��A���A���A�K�A�t�A�  A��#A���A���A�  A�XA���A�n�A�`BAĺ^A���A���A��A�5?A�"�A��+A�M�A���A���A�l�A�n�A��A��^A��PA�A��HA��A�\)A��A�1A�A�dZA�oA��A�A��yA�33A��mA�A�A���A�-A��;A���A���A���A�"�A�+A���A��A�I�A���A��A�{A�ƨA�G�AƑhAđhA���A�G�A�p�A��^A��Aİ!AƲ-A��A��A���A��A�=qA�^5A�=qAƩ�A�z�A�v�A�33A��A��A�1A�ffA���A���A�A�  A��yA��HA��;A�A�A�  A��A���A�x�A�(�A�~�A�ƨA�VA�%A�AƋDA�bNA��A�Q�A��`A���A�K�A���A�1A���A��A�  A�JA�
=A�ĜA��HA�jA���A�
=A�oAƗ�AđhA��!A��A�33A�oA�bA�jA�O�A�JA�JA�bA�VA�
=A��9A�%A�{A�bA�bA�1A��/A��jA��A�VA���A��A�  A���A���A���A���A�5?A�1A�JA�1A�A�M�A�A�VA�oA�{A�A�%A�bA�bA���A�A��A�bA�%A�VA�oA�JA�VA�JA��A�{A�oA�{A�{A�{A�oA�oA�{A��A��A�{A��A�{A�bA�bA�VA�VA�1A�JA�VA�1A�1A�1A�
=A�JA�{A�oA�JA�
=A�
=A�%A�1A�  A�JA�
=A�%A���A�
=A�
=A�JA�
=A�VA�JA�
=A�JA�
=A�bA�VA�VA�VA�VA�VA�oA�bA�VA�JA�
=A�bA�VA�JA�bA�bA�oA�bA�VA�{A�{A��A�bA�bA�JA�A�A�A�bA�bA�VA�JA�A�VA�VA�JA�
=A�bA�VA�VA�JA�JA�A�JA�
=A�JA�%A�
=A�JA�
=A�
=A�
=A�
=A�
=A���A�JA�JA�%A�JA�
=A�
=A�1A��A�JA�1A�
=A���A�XA�
=A�
=A�1A�JA�VA�oA�%A�
=A�
=A�
=A�VA�1A�JA�1A�VA�
=A�%A�1A�1A�A�VA�A�%A�A�A�A�1A�JA�JA�1A�A�JA�A�bA�JA�bA�{A���A�bA�
=A�{A�bA�VA�JA�bA�VA�bA�%A�
=A�%A�1A�{A�oA�1A�%A�bA�VA�
=A�VA�VA�bA�oA�VA�bA�VA�VA�JA�1A�bA�bA�VA�bA�bA�{A�VA�VA�VA�VA�VA�bA�bA�VA�VA�VA�VA�bA�VA�VA�VA�VA�bA�bA�VA�oA�VA�VA�VA�bA�VA�VA�JA�VA�
=A�1A�1A�1A�A�
=A�
=A�{A�bA�JA�JA�bA�oA�oA�{A��A��A�{A��A�$�A�$�A� �A� �A�"�A�$�A�"�A�&�A�&�A�&�A�"�A�"�A�&�A�&�A�&�A�(�A�&�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�+A�+A�(�A�(�A�(�A�(�A�+A�+A�(�A�+A�+A�&�A�&�A�&�A�$�A�$�A�&�A�$�A�$�A�"�A�&�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�$�A� �A��A��A��A��A��A��A��A� �A� �A� �A� �A�"�A�$�A�"�A� �A� �A��A� �A�"�A� �A� �A�"�A� �A� �A�"�A�&�A�$�A�$�A�$�A�&�A�+A�+A�+A�+A�(�A�+A�+A�-A�-A�+A�+A�-A�-A�-A�/A�1'A�/A�33A�5?A�33A�33A�5?A�33A�33A�33A�/A�/A�/A�-A�1'A�/A�-A�/A�-A�/A�1'A�/A�1'A�1'A�1'A�1'A�33A�33A�5?A�33A�33A�33A�1'A�5?A�33A�5?A�1'A�33A�33A�33A�33A�33A�/A�1'A�1'A�/A�1'A�1'A�/A�/A�1'A�1'A�1'A�33A�1'A�/A�/A�/A�-A�/A�-A�-A�-A�33A�1'A�33A�9XA�7LA�9XA�7LA�7LA�5?A�;dA�9XA�9XA�9XA�9XA�7LA�7LA�9XA�9XA�5?A�9XA�7LA�7LA�9XA�7LA�7LA�7LA�9XA�;dA�;dA�7LA�9XA�9XA�9XA�9XA�;dA�7LA�9XA�;dA�9XA�;dA�;dA�;dA�9XA�9XA�9XA�9XA�;dA�=qA�;dA�;dA�9XA�9XA�;dA�;dA�9XA�;dA�9XA�;dA�9XA�=qA�;dA�;dA�;dA�9XA�;dA�=qA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�?}A�=qA�=qA�=qA�;dA�=qA�9X@�t�@�t�@�dZ@�C�@�C�@�33@�;d@�33@�+@�+@�+@�33@�33@�;d@�33@�33@�33@�+@�+@�33@�33@�"�@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@��@�"�@��@��@�"�@�"�@��@�"�@��@�"�@�"�@�"�@�"�@�"�@�o@�o@�o@�o@�o@�o@�o@�o@�o@�o@��@��@��@�o@�o@��@��@��@�o@�o@�o@�o@�
=@�@�
=@�
=@�
=@���@���@��@���@���@��@��@��@�ȴ@���@��!@��R@��!@��\@��\@��\@��\@��+@��+@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�~�@�v�@�~�@�v�@�~�@�~�A�&�A�$�A�$�A�$�A�&�A�&�A�-A�+A�+A�(�A�(�A� �A�"�A� �A��A��A��A��A��A��A��A��A� �A��A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A�"�A�$�A�"�A� �A� �A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�+A�+A�-A�-A�-A�/A�+A�-A�-A�+A�-A�/A�/A�/A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�5?A�5?A�5?A�5?A�1'A�1'A�5?A�1'A�1'A�33A�33A�33A�33A�33A�1'A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�1'A�1'A�33A�1'A�/A�/A�/A�1'A�1'A�/A�-A�/A�5?A�33A�9XA�9XA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�7LA�7LA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=q@�t�@�t�@�t�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�33@�;d@�;d@�33@�33@�33@�33@�33@�33@�33@�+@�+@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�+@�"�@�+@�"�@��@�"�@��@��@�"�@�"�@�"�@�"�@�"�@��@�"�@��@��@�o@�o@�o@�o@�o@�o@��@�o@��@��@��@��@�o@��@��@��@��@��@�o@�o@�o@�
=@�
=@�
=@�
=@�@�@��y@��@��@��@���@���@���@���@��R@��R@��R@���@��\@��\@��\@��\@��+@��+@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�$�A�"�A�&�A�1'A�/A�33A�1'A�33A�9XA�9XA�;dA�;dA�=qA�=qA�=qA�=qA�;dA�;dA�33A�/A�1'A�-A�(�A��A���A���A�VA�t�A���A���A���A�ƨA�ƨA�t�A�K�A�1'A��FA��
A�I�A��A���A���A�33A���A���A�v�A�n�A�%A���A��uA�M�A�VA�ĜA���A��A�jA�{A���A���A���A���A���A��hA�r�A�7LA�"�A��A��A��A�XA� �A��A���A��A���A���A�x�A�"�A�%A���A��uA�r�A�^5A�^5A�Q�A�M�A�E�A���A���A�bNA�JA�\)A��A�ffA��A��A�XA���A�\)A��A��uA��uA�A���A��FA�A�A��\A��A���A�M�A���A�  A���A�(�A� �A���A���A�bA��jA�XA�~�A��!A�;dA�`BA~~�A|1'AxZAu��Am�;Aj��Ag��AdM�Ab��A^ȴA\�`A[�
A[�AY�^AWG�AT�AS�TAS?}AQ��AQ%AO�mAOVAM;dAKK�AI`BAGAF�9AF�DAEp�AD��AC��AB��A@��A?�A>ȴA=\)A<5?A;`BA8�A65?A3�A2��A0��A/O�A-�^A*�A(~�A'�A&�`A&{A%��A$ȴA$1A#"�A ffA�TAhsA��A��Az�A��AXA�A?}AA�A\)A��AZA�-A�`A��A��A��AC�AM�AhsAx�A\)A7LA�/A�A�A �A�TAdZA�-A	`BA+A�`A��AffAS�AĜA=qAt�A�FA =q@��+@��7@�%@���@���@��@�J@���@�{@��@��T@�9X@�F@�ȴ@�$�@�X@�C�@��@�Z@���@�?}@�|�@�"�@���@�V@ؼj@�Z@և+@��@Ͼw@�J@�`B@��@̛�@��@��@���@˥�@�K�@ʟ�@�?}@ț�@�1'@��@�@ēu@�9X@�t�@�X@�b@�dZ@��T@��@�K�@��@��!@��7@��/@�Q�@�(�@�1@���@��@��m@��@�l�@�+@�~�@�J@�G�@�&�@�V@���@�|�@���@�^5@�7L@�r�@�I�@��@�;d@�-@�@�J@�-@�=q@��@���@�%@�Q�@�t�@�ȴ@��@��-@��`@��w@�t�@�+@�"�@��H@���@�@��h@�`B@��@���@��@���@�z�@�9X@�(�@��m@�K�@�33@��H@���@�X@��j@�I�@�1@�|�@��y@�n�@�-@��7@�V@��u@�Z@��@���@�G�@���@��9@�z�@���@���@���@��;@��;@��@��@���@���@�Ĝ@���@�33@�@��y@�l�@��\@��T@��h@��@�O�@�/@�&�@���@���@�Ĝ@�Ĝ@��@��D@��F@���@�dZ@�C�@�
=@��y@��y@�+@�33@�dZ@��y@�ff@��R@���@�~�@�5?@�{@���@��T@��-@�p�@�`B@�O�@��/@��@��;@�  @�  @���@�ff@�=q@�{@��@�$�@�{@��@���@���@�p�@��7@�x�@�`B@�X@�&�@���@���@���@�Z@� �@�1@��@��;@���@�dZ@�\)@�+@��H@��!@���@���@�n�@�^5@�E�@�5?@�-@�J@���@���@���@�@�@���@�X@�7L@��`@��D@�1'@��F@�l�@�C�@�33@�
=@��H@���@��h@�hs@�`B@�`B@�&�@�%@���@��`@��j@��u@�Z@�(�@��@��@�33@�"�@�"�@��@�@���@��+G�O�@�U�@z@o9�@d�@\Z@U�X@N;�@F^5@@A�@;+@2��@.�B@*v�@%<6@ e�@)_@�@��@i�@
p;@dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A�  A��A��uA�E�A�oA��A��A��A�A���A�7LAăA�  AƶFA�K�A���A��A�$�A��Aŉ7A���A���A�XA��`A���A��A�jA��mAÍPAŲ-A�+A�E�A�JA�l�A�33A��hA�bA�Q�A�t�A�VA��A�ZA���A���A�?}A�bA�7LA��A��A���A���A���A���A���A��A���A���A�K�A�t�A�  A��#A���A���A�  A�XA���A�n�A�`BAĺ^A���A���A��A�5?A�"�A��+A�M�A���A���A�l�A�n�A��A��^A��PA�A��HA��A�\)A��A�1A�A�dZA�oA��A�A��yA�33A��mA�A�A���A�-A��;A���A���A���A�"�A�+A���A��A�I�A���A��A�{A�ƨA�G�AƑhAđhA���A�G�A�p�A��^A��Aİ!AƲ-A��A��A���A��A�=qA�^5A�=qAƩ�A�z�A�v�A�33A��A��A�1A�ffA���A���A�A�  A��yA��HA��;A�A�A�  A��A���A�x�A�(�A�~�A�ƨA�VA�%A�AƋDA�bNA��A�Q�A��`A���A�K�A���A�1A���A��A�  A�JA�
=A�ĜA��HA�jA���A�
=A�oAƗ�AđhA��!A��A�33A�oA�bA�jA�O�A�JA�JA�bA�VA�
=A��9A�%A�{A�bA�bA�1A��/A��jA��A�VA���A��A�  A���A���A���A���A�5?A�1A�JA�1A�A�M�A�A�VA�oA�{A�A�%A�bA�bA���A�A��A�bA�%A�VA�oA�JA�VA�JA��A�{A�oA�{A�{A�{A�oA�oA�{A��A��A�{A��A�{A�bA�bA�VA�VA�1A�JA�VA�1A�1A�1A�
=A�JA�{A�oA�JA�
=A�
=A�%A�1A�  A�JA�
=A�%A���A�
=A�
=A�JA�
=A�VA�JA�
=A�JA�
=A�bA�VA�VA�VA�VA�VA�oA�bA�VA�JA�
=A�bA�VA�JA�bA�bA�oA�bA�VA�{A�{A��A�bA�bA�JA�A�A�A�bA�bA�VA�JA�A�VA�VA�JA�
=A�bA�VA�VA�JA�JA�A�JA�
=A�JA�%A�
=A�JA�
=A�
=A�
=A�
=A�
=A���A�JA�JA�%A�JA�
=A�
=A�1A��A�JA�1A�
=A���A�XA�
=A�
=A�1A�JA�VA�oA�%A�
=A�
=A�
=A�VA�1A�JA�1A�VA�
=A�%A�1A�1A�A�VA�A�%A�A�A�A�1A�JA�JA�1A�A�JA�A�bA�JA�bA�{A���A�bA�
=A�{A�bA�VA�JA�bA�VA�bA�%A�
=A�%A�1A�{A�oA�1A�%A�bA�VA�
=A�VA�VA�bA�oA�VA�bA�VA�VA�JA�1A�bA�bA�VA�bA�bA�{A�VA�VA�VA�VA�VA�bA�bA�VA�VA�VA�VA�bA�VA�VA�VA�VA�bA�bA�VA�oA�VA�VA�VA�bA�VA�VA�JA�VA�
=A�1A�1A�1A�A�
=A�
=A�{A�bA�JA�JA�bA�oA�oA�{A��A��A�{A��A�$�A�$�A� �A� �A�"�A�$�A�"�A�&�A�&�A�&�A�"�A�"�A�&�A�&�A�&�A�(�A�&�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�+A�+A�(�A�(�A�(�A�(�A�+A�+A�(�A�+A�+A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�-A�+A�+A�(�A�(�A� �A�"�A� �A��A��A��A��A��A��A��A��A� �A��A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A�"�A�$�A�"�A� �A� �A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�+A�+A�-A�-A�-A�/A�+A�-A�-A�+A�-A�/A�/A�/A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�5?A�5?A�5?A�5?A�1'A�1'A�5?A�1'A�1'A�33A�33A�33A�33A�33A�1'A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�1'A�1'A�33A�1'A�/A�/A�/A�1'A�1'A�/A�-A�/A�5?A�33A�9XA�9XA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�7LA�7LA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=q@�t�@�t�@�t�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�33@�;d@�;d@�33@�33@�33@�33@�33@�33@�33@�+@�+@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�+@�"�@�+@�"�@��@�"�@��@��@�"�@�"�@�"�@�"�@�"�@��@�"�@��@��@�o@�o@�o@�o@�o@�o@��@�o@��@��@��@��@�o@��@��@��@��@��@�o@�o@�o@�
=@�
=@�
=@�
=@�@�@��y@��@��@��@���@���@���@���@��R@��R@��R@���@��\@��\@��\@��\@��+@��+@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�A�&�A�$�A�$�A�$�A�&�A�&�A�-A�+A�+A�(�A�(�A� �A�"�A� �A��A��A��A��A��A��A��A��A� �A��A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A�"�A�$�A�"�A� �A� �A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�+A�+A�-A�-A�-A�/A�+A�-A�-A�+A�-A�/A�/A�/A�1'A�33A�33A�33A�33A�33A�33A�33A�33A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�5?A�5?A�5?A�5?A�1'A�1'A�5?A�1'A�1'A�33A�33A�33A�33A�33A�1'A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�1'A�1'A�33A�1'A�/A�/A�/A�1'A�1'A�/A�-A�/A�5?A�33A�9XA�9XA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�7LA�7LA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=q@�t�@�t�@�t�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�33@�;d@�;d@�33@�33@�33@�33@�33@�33@�33@�+@�+@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�+@�"�@�+@�"�@��@�"�@��@��@�"�@�"�@�"�@�"�@�"�@��@�"�@��@��@�o@�o@�o@�o@�o@�o@��@�o@��@��@��@��@�o@��@��@��@��@��@�o@�o@�o@�
=@�
=@�
=@�
=@�@�@��y@��@��@��@���@���@���@���@��R@��R@��R@���@��\@��\@��\@��\@��+@��+@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@��+@��+@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�k=�Pr>ݭ@�}@w<6@&EN=� q=濇>*�@�f@�@
x�>�V�@��@��=�c�=���=�X%=�y>.)t@�9�>08\=�f�>&-@�ں@��@��=��v?�ȟ@*?��o>Ӫz>�`?��*>9�1@�_=��?R>>���?���>J��>"�9@��=K��=lo=��=�wG=˒:>�%�=�oi>D�u@�f@�=�>uy@) @��=���>/ i?H�@�@��>9C>7��@]Z�=�2�>a@8eA>^@? �@��=�7v=���>��@J �>�>���@��@�m?���=�Q/>o��@T�>!�!@�W @��?A�>�x�@��?E>	/o@H��>"��>R�@��@�m>"�@��;@�U@�@��>7��?��@��@�
R@��z@���>.;�@�E?�;>dy�@�/@�\>�~@�a�>�@��^@��=�!>A�?�5�=���>�"?Z'�@��>"�A@��.@�f@�?TV�>k�@_��@�|?V�@��>~%�@��>+��@���>��>\�>%_@�@��T@���=�>o�6@�@�m@�L@�4�>]d>���@I%F@� �>]�>kSP@��>u?}@8�K@��?@�>h��@��@��?�S�?^��?��@� �=�>=6�@� @��@��>H$@��@��@�"@�!B@�Z>��??ֶ@��@ g�@�"h@��@��>$J@���@�")@�"�@�"�@� \?�'>N{@�#�@�#@�#%@�!l@��#>|�U@�$�@�$�@��@��U@�"@�!�@��@��?�N'?@� �@�"h@�!l@��@���@�!@�#�@�$@�#�@� \@�!@�#%@�#@��@��@�$5@�#�@�"�@�$5@�$5@�!B@�#%@�#�@�%�@�%�@�%�@�%F@�$�@�%p@�%�@�%�@�&�@�&�@�&B@�%F@�&B@�$�@�%F@�%�@�$�@�#�@�%F@�$�@�#%@�#d@�%�@�"@�"�@�#%@�#d@�"�@�"h@�"�@�"�@�"�@�"@�"@�#@�!�@�!@�!�@�!�@� �@�!�@�!�@�"S@�"h@�"@�"h@�"@�#%@�#�@�$5@�$t@�$5@�%F@�%F@�%F@�$�@�$5@�#%@�#�@�$�@�#�@�$�@�$�@�$�@�$�@�%F@�%�@�$�@�$�@�#�@�$�@�$@�%�@A�z@�$5@�$�@�$5@�"h@�"�@�#%@�#@�"h@�"@�#�@�#�@�#d@�#%@�#%@� �@�#%@�"�@�#%@�"�@�"S@�#%@�"�@�"�@�"h@�"�@�"@�"h@�!�@�!l@�#%@�"�@�"h@�!�@�#%@��m@�"@�"@�#@�"h?�d�@�"h@�#�@�!�@�!�@�#%@�#%@�!B@� �@�!�@�!�@�#�@�"S@�"@�"@�!�@�!l@� @� \@� \@� G@�!l@�!l@�"@��@� @�:@�!l@�$t@�$�@�#�@�#�@�#%@�"h@�#�@�$�@�$t@�"�@�$5@�%F@�%�@�$�@�$�@�$�@�$@�#�@�$�@�#�@�#�@�$�@�!�@�$�@�%@�%F@�!@�$5@�$�@�$t@�#�@�$5@�$@�$�@�$�@�%F@�%�@�%@�$�@�$�@�%@�$�@�$�@�$�@�%F@�%p@�%F@�$�@�$�@�$�@�$5@�$�@�%F@�$@�#�@�#�@�$5@�$�@�$�@�$5@�$�@�%@�$�@�$5@�$�@�$5@�%F@�#�@�#�@�$t@�#�@�#�@�#%@�#�@�#�@�"h@�"h@�!�@�"�@�"h@�#�@�%F@�&B@�%�@�&�@�&-@�&�@�&B@�&�@�)�@�(�@�+�@�*�@�+@�-M@�-M@�-�@�-�@�-�@�-�@�.
@�.�@�.
@�.
@�-�@�.
@�.
@�.�@�/@�.�@�.�@�.�@�.�@�/@�/@�/@�/o@�/@�/@�/@�/�@�/�@�/�@�/�@�/�@�/�@�/�@�/�@�0�@�0+@�0+@�/�@�/�@�/�@�.�@�.�@�.�@�.s@�.�@�.�@�.4@�.s@�.s@�/@�/@�/@�/�@�/�@�.�@�-�@�0+@�+�@�+�@�+�@�+A@�+A@�+k@�+�@�,|@�,R@�,R@�,�@�-b@�-b@�-b@�-�@�-b@�,�@�-b@�-b@�-b@�-�@�-�@�-b@�-�@�-@�-�@�.s@�.�@�.
@�.�@�.�@�/0@�1f@�1�@�0�@�0�@�0�@�1�@�2a@�1�@�2a@�2@�3r@�3@�3	@�4n@�4n@�4n@�4�@�5�@�5~@�6;@�6;@�5+@�5?@�5�@�5~@�4@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�5+@�5�@�6;@�5�@�6;@�6�@�7a@�7a@�7a@�7�@�7a@�6�@�7a@�7�@�7a@�8	@�6�@�7a@�7a@�6�@�6�@�6�@�6�@�6�@�7�@�6�@�7a@�6�@�6�@�7@�7�@�7@�7�@�8@�7a@�7@�8@�8@�8@�7a@�7a@�7�@�8q@�9�@�:�@�:?@�;�@�;�@�;�@�;�@�;�@�;:@�<K@�;�@�;�@�;�@�;�@�<`@�<�@�<�@�<�@�<`@�<�@�<`@�=@�=@�=�@�=q@�=�@�=�@�>@�=�@�=q@�=�@�>@�>@�>�@�>-@�>�@�>�@�?)@�>�@�>�@�>�@�?)@�?)@�?)@�?)@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�?�@�@:@�?�@�@�@�@O@�@:@�@�@�@�@�@�@�@�@�A_@�A�@�B@�B@�A�@�B@�B[@�B@�B@�Bp@�B�@�Bp@�Bp@�Bp@�B�@�B�@�B�@�B�@�C-@P��@P�>@P�y@P�O@P�}@P��@P�X@P��@P��@P��@P�@P��@P�)@P�@P�@P�X@P��@P��@P��@P��@P��@P��@P�@P��@P��@P��@P��@P�\@P��@P��@P�\@P��@P�@P��@P�.@P��@P��@P�@P�2@P��@P�.@P��@P�@P��@P��@P��@P��@P�@P�.@P�\@P��@P�@P��@P�2@P�\@P��@P�\@P��@P��@P�@P��@P�@P��@P��@P�@P��@P��@P��@P��@P�\@P�2@P�`@P�;@P��@P��@P�?@P�?@P�H@P��@P�Y@P�Y@P��@P�@P�^@P��@P�@P��@P��@P�@P��@P�,@P�@P�,@P�@P��@P�,@P��@P��@P�@P��@P��@P�[@P�[@P�1@P�@P��@P�@P�@P�,@P�V@P��@P�V@P�@P��@P�,@P��@P�,@P�@P�,@P��@�d�@�c�@�cs@�c�@�d0@�d�@�f<@�ff@�f<@�d�@�eA@�a�@�c@�c@�`�@�ag@�`�@�`k@�`�@�`k@�`�@�a(@�a�@�a�@�b�@�c@�c5@�c @�c@�a�@�b$@�b$@�c @�c5@�c^@�c5@�bN@�b�@�c5@�cs@�c�@�c�@�d@�c�@�d�@�eA@�g#@�ff@�g#@�e�@�f�@�h�@�g8@�gw@�g�@�gb@�h�@�i/@�h�@�iD@�i�@�k@�k{@�k{@�j�@�k{@�k<@�kQ@�k�@�iD@�i�@�j+@�i�@�j@�i�@�j@�i�@�i�@�j@@�jj@�j@�kQ@�k@�kQ@�k�@�l�@�l�@�m	@�l�@�k�@�l7@�m@�kf@�k�@�l7@�lv@�l�@�lv@�lv@�l@�k�@�l�@�l@�k�@�l@�la@�l@�l7@�lL@�m	@�l�@�l�@�l�@�l�@�k�@�lv@�lL@�l�@�l�@�l�@�l@�l�@�n�@�n@�pP@�p�@�q@�q@�p�@�p�@�p�@�p�@�qL@�q"@�q"@�q@�qa@�q�@�q�@�q�@�p�@�q"@�qL@�q�@�q�@�r\@�rG@�rG@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�s@�sC@�sC@�s�@�s�@�s�@�s�@�s�@�s�@�t @�s�@�t@�t*@�ti@�t~@�tT@�tT@�t*@�t*@�tT@�tT@�t�@�ti@�t@�t*@�tT@�tT@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�u�@�ud@�u�@�u�@�u�@�v!@�vK@�vK@�vK@�vu@�v�@�v`@�v�@�vK@�v�@�v!@�vK@�vK@P�@P�.@P�@P��@Pߤ@P��@P�+@P��@Pݭ@P�]@P܇@P݃@P��@P�U@P�/@P�@Pݭ@P�@P�@P�U@P�+@P�@P݃@P�@P�@P��@P��@P�@Pܱ@Pܱ@P��@P܇@P�/@P�/@P܇@P�+@Pܱ@P�@P݃@P��@P�@Pܱ@P�@P�Y@P��@P�@P��@P��@P݃@P��@P��@P܇@P��@P��@P�]@P�]@P܇@Pܱ@P�Y@P��@P�@P�U@P��@P�@P�/@P�+@P�@P�@P�@P��@P��@P�@P��@P�8@P�b@P۶@P�3@P۶@P�8@P�E@PԀ@P��@P�|@P�V@P��@Pэ@Pэ@P�h@Pϖ@P�>@P�}@P�.@P��@P�.@P��@P��@P��@P�@P��@P�\@PɆ@P�2@P�@P�\@P�2@PɆ@Pɰ@P��@P�@P�@P�X@P�.@P�.@P�.@P�@Pɰ@P��@Pɰ@P�@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          3444344443344334444434443334444444434444443444444443344434443344344444344434433444343344344444334333344333343443343433444444343334433434343444333443333444344344344334443443334333334434333433333443333343333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�nG�O�G�O�G�O�@w<6G�O�G�O�G�O�G�O�@�f@��G�O�G�O�@��@��G�O�G�O�G�O�G�O�G�O�@�9�G�O�G�O�G�O�@�ھ@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�]G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�f@�G�O�G�O�G�O�@��G�O�G�O�G�O�@�@��G�O�G�O�@]Z�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@J �G�O�G�O�@��@�oG�O�G�O�G�O�@T�G�O�@�W@��G�O�G�O�@��!G�O�G�O�G�O�G�O�G�O�@��@�kG�O�@��>@�S@�@��G�O�G�O�@��@�
N@��{@���G�O�@�>G�O�G�O�@�.@�ZG�O�@�a�G�O�@��\@��G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�@��/@�f@�G�O�G�O�@_��@�~G�O�@��G�O�@��G�O�@���G�O�G�O�G�O�@�@��S@���G�O�G�O�@�@�u@�I@�4�G�O�G�O�G�O�@� �G�O�G�O�@��G�O�G�O�@��G�O�G�O�@��@��G�O�G�O�G�O�@� �G�O�G�O�@� 	@��@��G�O�@��@��@�"@�!F@�ZG�O�G�O�@��G�O�@�"j@��@��G�O�@���@�"(@�"�@�"�@� \G�O�G�O�@�#�@�#@�#%@�!o@��"G�O�@�$�@�$�@��@��W@�"@�!�@��@��G�O�G�O�@� �@�"h@�!n@��@���@�!@�#�@�$@�#�@� \@�!@�#%@�#@��@��@�$9@�#�@�"�@�$9@�$6@�!F@�#$@�#�@�%�@�%�@�%�@�%J@�$�@�%o@�%�@�%�@�&�@�&�@�&D@�%F@�&A@�$�@�%J@�%�@�$�@�#�@�%D@�$�@�#'@�#b@�%�@�"@�"�@�#%@�#j@�"�@�"g@�"�@�"�@�"�@�"@�"@�#@�!�@�!@�!�@�!�@� �@�!�@�!�@�"R@�"h@�"@�"g@�"@�#%@�#�@�$8@�$w@�$9@�%H@�%F@�%J@�$�@�$2@�#$@�#�@�$�@�#�@�$�@�$�@�$�@�$�@�%H@�%�@�$�@�$�@�#�@�$�@�$@�%�G�O�@�$6@�$�@�$8@�"h@�"�@�#$@�#@�"g@�"@�#�@�#�@�#f@�#%@�#%@� �@�#"@�"�@�#%@�"�@�"R@�#%@�"�@�"�@�"j@�"�@�"@�"f@�!�@�!j@�#'@�"�@�"g@�!�@�#(@��o@�"@�"@�#@�"kG�O�@�"g@�#�@�!�@�!�@�#'@�# @�!B@� �@�!�@�!�@�#�@�"U@�"@�"@�!�@�!r@� @� _@� \@� J@�!o@�!j@�"@��@� 	@�:@�!n@�$y@�$�@�#�@�#�@�#*@�"f@�#�@�$�@�$w@�"�@�$8@�%J@�%�@�$�@�$�@�$�@�$@�#�@�$�@�#�@�#�@�$�@�!�@�$�@�%@�%F@�!@�$8@�$�@�$v@�#�@�$8@�$@�$�@�$�@�%H@�%�@�%@�$�@�$�@�%@�$�@�$�@�$�@�%J@�%t@�%F@�$�@�$�@�$�@�$8@�$�@�%H@�$@�#�@�#�@�$8@�$�@�$�@�$9@�$�@�%@�$�@�$8@�$�@�$8@�%F@�#�@�#�@�$v@�#�@�#�@�#'@�#�@�#�@�"j@�"d@�" @�"�@�"g@�#�@�%J@�&F@�%�@�&�@�&/@�&�@�&B@�'@�)�@�(�@�+�@�*�@�+@�-M@�-M@�-�@�-�@�-�@�-�@�.
@�.�@�.@�.
@�-�@�.@�.@�.�@�/@�.�@�.�@�.�@�.�@�/@�/@�/@�/q@�/@�/@�/@�/�@�/�@�/�@�/�@�/�@�/�@�/�@�/�@�0�@�0.@�0.@�/�@�/�@�/�@�.�@�.�@�.�@�.l@�.�@�.�@�d�@�c�@�ct@�c�@�d2@�d�@�f<@�fg@�f?@�d�@�eB@�a�@�c
@�c@�`�@�ac@�`�@�`l@�`�@�`l@�`�@�a(@�a�@�a�@�b�@�c@�c6@�c#@�c@�a�@�b&@�b"@�c @�c3@�c\@�c2@�bN@�b�@�c8@�cq@�c�@�c�@�d@�c�@�d�@�eB@�g'@�fe@�g#@�e�@�f�@�h�@�g:@�gu@�g�@�ge@�h�@�i0@�h�@�iF@�i�@�k@�kz@�kz@�j�@�kw@�k?@�kR@�k�@�iD@�i�@�j-@�i�@�j@�i�@�j@�i�@�i�@�jA@�jj@�j@�kN@�k@�kR@�k�@�l�@�l�@�m	@�l�@�k�@�l8@�m#@�kf@�k�@�l7@�ly@�l�@�ly@�lv@�l@�k�@�l�@�l@�k�@�l@�la@�l@�l8@�lN@�m
@�l�@�l�@�l�@�l�@�k�@�lv@�lN@�l�@�l�@�l�@�l@�l�@�n�@�n@�pQ@�p�@�q@�q@�p�@�p�@�p�@�p�@�qK@�q"@�q"@�q@�qa@�q�@�q�@�q�@�p�@�q!@�qN@�q�@�q�@�r^@�rC@�rF@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�s@�sB@�sB@�s�@�s�@�s�@�s�@�s�@�s�@�s�@�s�@�t@�t(@�tj@�t~@�tT@�tW@�t+@�t(@�tR@�tQ@�t�@�ti@�t@�t.@�tT@�tT@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�u�@�ud@�u�@�u�@�u�@�v@�vM@�vK@�vM@�vv@�v�@�vc@�v�@�vK@�v�@�v!@�vP@�vN@P�~@P�+@P�@P��@Pߦ@P��@P�-@P��@Pݰ@P�[@P܆@P݈@P��@P�X@P�2@P�@Pݭ@P�~@P�@P�X@P�+@P�@P݂@P� @P�@P��@P��@P�@Pܰ@Pܰ@P��@P܃@P�2@P�0@P܆@P�+@Pܰ@P�@P݃@P��@P�@Pܵ@P�@P�Z@P��@P�@P��@P��@P݅@P��@P��@P܆@P��@P��@P�b@P�[@P܊@Pܰ@P�X@P��@P� @P�U@P��@P�@P�.@P�*@P� @Pޅ@P�~@P��@P��@P�@P��@P�6@P�e@P۶@P�.@Pۺ@P�8@P�F@PԀ@P��@P�{@P�Z@P��@Pю@Pѐ@P�f@Pϓ@P�=@P�}@P�.@P��@P�2@P��@P��@P��@P� @P��@P�^@PɊ@P�3@P�@P�[@P�2@PɈ@Pɰ@P��@P�@P�@P�X@P�.@P�0@P�-@P�@Pɰ@P��@Pɫ@P�@P��@�d�@�c�@�ct@�c�@�d2@�d�@�f<@�fg@�f?@�d�@�eB@�a�@�c
@�c@�`�@�ac@�`�@�`l@�`�@�`l@�`�@�a(@�a�@�a�@�b�@�c@�c6@�c#@�c@�a�@�b&@�b"@�c @�c3@�c\@�c2@�bN@�b�@�c8@�cq@�c�@�c�@�d@�c�@�d�@�eB@�g'@�fe@�g#@�e�@�f�@�h�@�g:@�gu@�g�@�ge@�h�@�i0@�h�@�iF@�i�@�k@�kz@�kz@�j�@�kw@�k?@�kR@�k�@�iD@�i�@�j-@�i�@�j@�i�@�j@�i�@�i�@�jA@�jj@�j@�kN@�k@�kR@�k�@�l�@�l�@�m	@�l�@�k�@�l8@�m#@�kf@�k�@�l7@�ly@�l�@�ly@�lv@�l@�k�@�l�@�l@�k�@�l@�la@�l@�l8@�lN@�m
@�l�@�l�@�l�@�l�@�k�@�lv@�lN@�l�@�l�@�l�@�l@�l�@�n�@�n@�pQ@�p�@�q@�q@�p�@�p�@�p�@�p�@�qK@�q"@�q"@�q@�qa@�q�@�q�@�q�@�p�@�q!@�qN@�q�@�q�@�r^@�rC@�rF@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�s@�sB@�sB@�s�@�s�@�s�@�s�@�s�@�s�@�s�@�s�@�t@�t(@�tj@�t~@�tT@�tW@�t+@�t(@�tR@�tQ@�t�@�ti@�t@�t.@�tT@�tT@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�u�@�ud@�u�@�u�@�u�@�v@�vM@�vK@�vM@�vv@�v�@�vc@�v�@�vK@�v�@�v!@�vP@�vN@P�~@P�+@P�@P��@Pߦ@P��@P�-@P��@Pݰ@P�[@P܆@P݈@P��@P�X@P�2@P�@Pݭ@P�~@P�@P�X@P�+@P�@P݂@P� @P�@P��@P��@P�@Pܰ@Pܰ@P��@P܃@P�2@P�0@P܆@P�+@Pܰ@P�@P݃@P��@P�@Pܵ@P�@P�Z@P��@P�@P��@P��@P݅@P��@P��@P܆@P��@P��@P�b@P�[@P܊@Pܰ@P�X@P��@P� @P�U@P��@P�@P�.@P�*@P� @Pޅ@P�~@P��@P��@P�@P��@P�6@P�e@P۶@P�.@Pۺ@P�8@P�F@PԀ@P��@P�{@P�Z@P��@Pю@Pѐ@P�f@Pϓ@P�=@P�}@P�.@P��@P�2@P��@P��@P��@P� @P��@P�^@PɊ@P�3@P�@P�[@P�2@PɈ@Pɰ@P��@P�@P�@P�X@P�.@P�0@P�-@P�@Pɰ@P��@Pɫ@P�@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          3444344443344334444434443334444444434444443444444443344434443344344444344434433444343344344444334333344333343443343433444444343334433434343444333443333444344344344334443443334333334434333433333443333343333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��L9��K9��9��t9���9��99���9���9���9��N9���9��P9���9���9���9��"9��p9��:9��a9��:9���9���9���9���9��M9���9���9���9���9���9���9���9���9���9���9���9���9��?9���9��9��99��s9���9���9��&9���9� �9���9� �9��u9� K9��9� �9� �9�L9� �9��9�u9�:9��9�9�:9��9��9��9��9�c9�u9��9��9�#9�b9�'9�99�&9�J9�)9�&9�u9��9��9�q9�99�u9��9��9��9�9��9��9�M9�*9��9�9�L9��9��9��9��9�)9��9��9�&9�9�$9�s9�#9�M9�b9�9��9��9��9��9��9��9�b9��9��9��9�&9��9��9��9�	&9�	�9�	�9�	�9�	�9�	s9�	�9�	�9�
9�	�9�	�9�	�9�
%9�
�9�
�9�
�9�	�9�	�9�
9�
�9�
�9�9�
�9�
�9�q9��9�89�89�49�K9�N9��9��9��9�&9�&9�#9�89�F9�r9��9��9��9��9��9�9��9��9��9��9��9��9�'9��9��9��9��9��9�$9�'9�c9�s9�v9�s9�_9�9��9�K9�N9�99��9��9��9��9��9��9��9� 9��9��9��9��9��9D9D�9D9D�9D�9D�9DF9D
�9D
�9D	�9D	�9D
�9D�9Do9D
[9D9D
�9D�9D9Do9DD9D
.9D
�9D9D
39D
9D
	9D
19D	�9D	�9D
9D	�9D
[9D
Y9D	�9DD9D	�9D9D
�9D
9D
.9D	�9D
.9D
�9D
�9D9D
�9D
�9D
�9D
�9D
9D	�9D	 9D	9D	�9D	�9D	�9D	�9D
~9D
9D9Dl9D
�9D9D
W9DD9D9D�9D�9D
�9D
�9D
19D
�9D~9D�9D�9D	g9D�9D�9D�9D19D�9D9D9D�9C�n9C�p9C�X9C��9C�19C��9C��9C�19C��9C�39C�.9C�39C�V9C�39C��9C��9C��9C�r9C��9C��9C��9C�9C�19C�\9C�Y9C��9C��9C��9C��9C�X9C�9C�69C�9C�\9C�6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B33BiyBJB�B'�B+BhB��B��B'�BO�BaHBdZBgmBjBl�Bl�Bk�Bk�BjBiyBiyBgmBe`BdZBbNB`BB^5B]/B]/B[#BZBXBW
BVBVBVBVBS�BP�BO�BN�BN�BL�BE�BB�BH�BR�B_;BcTBffBffB`BB]/BW
BO�BJ�BG�BF�BD�BC�B@�B-BbB�B��B�dB�9B��B��B�PB|�Bk�BR�B7LB�B%B�B�/BB��B� BgmB.BB
�B
�mB
�BB
ƨB
�'B
��B
�bB
v�B
_;B
=qB
)�B
+B	�B	�TB	ƨB	�9B	��B	{�B	G�B	1'B	 �B	uB	1B�B�NB�/B�B��B��B�
B�TB�NB�B��BȴBĜB�wB�XB�RB�B��B��B�wB��B��B��BƨB��B�}B�jB�RB�3B��B��B��B��B��B��B��B�{B�\B�VB�VB�\B�bB�uB�oB�\B�oB�bB�\B�\B�VB�VB�PB�=B�1B�B�B�B� B|�By�Bt�Bp�Bl�Bk�BjBiyBgmBffBjBk�Bk�BiyBdZBe`BgmBgmBcTB]/BW
BP�BZB`BB^5BbNBe`Be`BffBbNB^5B^5B`BBbNBbNBbNBbNBaHBk�Bm�BdZBgmBhsBjBl�Bm�Bw�Bt�Bt�Br�Bn�Bk�BffB_;B_;BaHBcTBdZB^5B_;B]/B`BB_;B_;B^5B_;B_;B^5B]/B\)B\)B\)B[#BZB^5B_;BaHBcTBcTBe`BffBffBiyBl�Bo�Bq�Bq�Bu�Bv�Bz�B{�B}�B}�B~�B~�B� B�B�B�%B�1B�DB�DB�DB�DB��B��B��B��B��B��B�!B�'B�!B�'B�9B�FB�XB��BÖBŢBǮBɺB��B��B�B�#B�BB�NB�mB�B�B�B��B��B��B��B	B	B	B	B	B	%B	1B	DB	JB	VB	uB	uB	�B	�B	�B	�B	 �B	&�B	(�B	/B	33B	6FB	;dB	A�B	F�B	O�B	T�B	YB	\)B	_;B	cTB	dZB	l�B	o�B	q�B	s�B	r�B	n�B	jB	iyB	iyB	iyB	l�B	s�B	t�B	t�B	w�B	x�B	z�B	|�B	|�B	~�B	�B	�B	�B	�%B	�DB	�DB	�DB	�VB	�\B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�3B	�!B	�!B	�-B	�-B	�3B	�9B	�FB	�FB	�RB	�jB	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�)B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�AB	��B

�B
�B
$ZB
)�B
/�B
8�B
?�B
B�B
K�B
Q�B
T�B
Z�B
^�B
d&B
jKB
k�B
r�B
vFB
zxG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��>���?F�A]�^A��!A���>��?� ?Y�B��B��ARr�@q7B�~B�>��z>���>��?'�?a��B
�m?f�+?�??p�B�IB�B�(>�KA"lAU�@�F?@�?>��A4̪?oQB��?@��X@/�@ܭ�?�'|?R�)BMg>���>���>�>��X?��?��?��?}*RB��Bƺ?�|?7�AZF!B��>�?a\�@� }B� B�P??;C?o�A���>��?-AA���?�lA�aB�+>���>�D,?4�SA�μ?+�z?�
�B�0B�S@��$>�Tm?��]A��n?R�wB
nB�@?#>?��B�r@_ �?/n�A�˦?R_B?��TB��B��?N��A�4B��B��B�a?t˰@S��B��B�B5�B��?aSB�\A �?�{�B��B��?N�IA� �??��B��B�U?;�?|�~A-tP>߮�?:��@���B�m?R�$B	��B��B�@��?7�^A��	B�Q@�;B�.?���B��?\�	B�}@<�?2Xq?X=%B��B
{=Bky?�<?�5�B��B�SB��B�?M�r@&_A�(�BR�?1�?�*B� ?��nA��B�"@�$�?�VBŝB�'@�|�@��0@��OB��?_B?r�WB��B��A�C�?��B�BQB�CB�LB,�@#��@�b�B�pAAL�B�eB��B�	?;O�BC�B�B��B�]B��@8��?�k�B��B��B��B�jB	-L?�v�B�B�0B�PBf�B�CB��B�xB��A(a@Z_pB��B��B�rB�rB�eB��B�ZB��B��B�B��B��B��B��B�'B�sB�~B��B��B�B��B��B��B��B��B��B�DB��B�kB��B��B��B��B�RB�;B�kB��B��B�(B�+B�ZB�B��B��B�FB�LB��B��B�mB�{B��B��B��B��B��B�B�;B�ZB��B��B�rB��B��B�"B��B��B��B�2B��B�;B��B�IB��B��B��B��B�B��B��B�sB�AB�_B��B�-B�"B�"B��B�pB��B��B��B�B�~B�B�DB��A��oB��B�hB��B��B�CB��B��B��B�CB��B�QB��B�mB�mB�_B�vB��B�mB��B�uB�mB��B��B��B��B�;B��B�B��B��B�'B��B��B�Bl�B�pB�B�%B�x@��>B��B��B��B�*B��B�B�B��B��B��B�B�IB�pB�B��B��B��B�mB�}B��B�	B��B��B�B��B�B�rB��B��B��B��B�eB��B��B��B�B��B�B��B��B��B�hB��B�LB��B��B��B�8B��B��B��B�B�B�B��B�pB��B��B��B�xB�xB��B��B� B�vB��B�B��B�pB�+B�DB��B��B�;B��B�(B�;B��B�;B��B��B�IB�IB��B��B�B��B�DB�vB�+B��B�+B��B�B�QB�IB��B�vB�IB��B��B�AB�pB�;B��B��B��B�xB�B�B��B��B�B��B��B�vB�MB��B�GB�JB��B�B�B�B�B�BB�oB�B�^B��B��B�)B�oB��B��B��B��B�wB��B��B��B��B��B�/B��B��B��B�B�2B��B�gB�^B�!B�oB�gB�2B��B��B�$B��B�=B�$B�B�/B��B��B��B�MB��B�KB�HB�}B�tB�B�HB�B�7B�YB��B�EB�UB�qB�iB��B��B�|B��B��B�JB��B��B��B�$B��B�B��B�EB�zB��B��B�aB��B�B��B�zB�PB�=B��B��B�iB��B�B�gB�KB�B�/B��B�TB��B�B�\B�;B�B�YB��B��B��B�-B��B�jB�bB��B�_B��B��B��B�B�B��B�0B��B��B�B��B��B��B��B�mB�B�TB��B��B�xB��B��B�WB��B�B��B�-B��B�0B�B�B��B�;B��B�B�3B��B��B�sB�	B�vB��B��B��B�vB�B�B��B�yB�hB�+B��B�bB��B�FB��B��B�pB�RB�ZB��B��B��B�LB��B�UB�EB�<B�+B�OB��B��B��B��B��B�B��B��B�7B��B�B�JB��B�fB��B�B�^B�MB��B�}B�XB��B�EB�lB��B��B�B��B��B��B�B�BB��B�wB�oB�)B��B� B�=B� B�,B��B�B�$B�/B��B�KB�'B�KB��B��B��B�B�xB��B��B�_B��B��B��B��B��B�B��B��B��B��B��B��B��B��B	�VB	�4B	�wB	ɌB	��B	�QB	��B	�RB	�tB	�HB	ʊB	�B	�7B	�5B	�8B	�hB	�mB	�pB	�UB	�8B	ȑB	��B	ɷB	ɋB	ʎB	�UB	�gB	��B	� B	�B	��B	�
B	�B	ɳB	�B	ɪB	ʎB	��B	�)B	ʗB	ɪB	�_B	ʁB	ɳB	��B	ɘB	ɻB	�B	�&B	�qB	�)B	�:B	�B	�1B	�CB	�GB	�B	�LB	� B	�bB	ɡB	�*B	��B	��B	�B	��B	ȼB	ȯB	�gB	�;B	�B	�[B	ȇB	�LB	�"B	ǙB	ǌB	�!B	�eB	�eB	�[B	ʌB	��B	�]B	�B	ȵB	ȴB	��B	�/B	ǀB	�wB	�KB	�OB	�#B	��B	�8B	��B	˴B	�/B	̝B	�qB	�EB	�+B	��B	�~B	�B	�dB	�IB	�[B	�mB	�qB	�5B	��B	�<B	��B	̷B	��B	̮B	˰B	��B�4B�B��B�B�B�B�B��B��B�7B�B�\B��B�B�B��B�8B��B��B��B�aB�B�oB�'B�KB�B��B�B�B�EB�B�wB�B�B��B�B�uB�B�B��B�1B�cB�B�^B��B��B�B��B��B�B�B�1B�B��B�SB�B��B�B�GB�B�&B�B��B��B�.B��B�B�B��B� B��B�B�B�B�B�B�iB�`B�B��B��B�B�kB�B��B�LB�XB�vB�B�B�(B�XB�LB��B�+B�]B�B�LB�<B�B��B��B�|B�+B�cB�B�JB�hB�7B�GB�B�B�B�B�B�
B��B�tB�1B�.B�9B�B�B�B�AB�B��B��B�B�TB�>B�~B��B�B�ZB�>B�B�B�B�B��B�B�.B��B�B�6B�OB�FB�B��B�"B�B�B�B�B�B�B�B�B��B�B��B��B��B�B��B�B�B�{B�B�"B�B�B��B��B��B�B��B�B�B�B�B��B��B�.B�B��B��B��B�B�>B��B��B�B�.B�B�B�B�B�(B��B�B��B��B�B�B�B	�B	�QB	�tB	�$B	��B	�TB	��B	�B	�YB	�fB	�jB	�B	��B	�gB	�B	�B	��B	�TB	��B	�B	��B	�B	�PB	�B	��B	�B	��B	��B	�]B	�PB	�aB	�	B	�wB	�jB	��B	��B	��B	�B	�KB	��B	��B	�B	��B	��B	�B	�1B	��B	��B	��B	��B	�B	��B	�>B	�0B	�B	�eB	�wB	�B	��B	�B	�9B	�\B	��B	�B	�aB	��B	��B	�B	�B	�|B	�B	��B	�XB	�oB	�sB	�B	��B	�B	�#B	�B	�`B	�B	��B	�B	��B	�B	��B	�*B	�B	��B	�B	�B	�RB	�B	�7B	�;B	� B	�!B	��B	�B	�B	�gB	�-B	�^B	�%B	�UB	�gB	�ZB	�}B	�oB	�B	�VB	�YB	�?B	�B	��B	��B	�B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444344443344334444434443334444444434444443444444443344434443344344444344434433444343344344444334333344333343443343433444444343334433434343444333443333444344344344334443443334333334434333433333443333343333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B�qB�qB�hB�qB�qB�qB�sB�sB�oB�qB�qB�qB�qB�sB�qB�qB�wB�wB�B�}B�}B�B�B�B��B3BicB9B�B'�B*�BQB��B��B'�BO�Ba2BdABgXBjhBlvBlwBkqBkrBjgBifBibBgWBeHBdEBb8B`.B^ B]B]B[BZBW�BV�BU�BU�BU�BU�BS�BP�BO�BN�BN�BL�BE�BByBH�BR�B_$Bc?BfPBfMB`,B]BV�BO�BJ�BG�BF�BD�BC�B@iB,�BMB��BͿB�OB�&B��B��B�<B|�BknBR�B76B�BB�B�B�vB��B�BgWB-�B �B
�B
�WB
�*B
ƕB
�B
��B
�LB
v�B
_%B
=ZB
)�B
B	�B	�=B	ƒB	�#B	�fB	{�B	G�B	1B	 �B	]B	B�B�3B�B��B��B��B��B�8B�3B��B��BȚBăB�]B�?B�8B��B��B��B�]BͻBˮBʧBƌB�oB�cB�MB�7B�B��B��B��B��B��B��B��B�^B�CB�<B�;B�AB�GB�ZB�TB�?B�SB�FB�AB�@B�:B�:B�3B�"B�B�B��B��B�B|�By�Bt�Bp�BlnBkiBjcBi]BgPBfIBjcBkgBkiBi\Bd@BeBBgPBgQBc6B]BV�BP�BZB`&B^Bb1BeEBeBBfHBb1B^B^B`$Bb1Bb1Bb.Bb2Ba)BkiBmtBd<BgRBhUBj^BloBmsBw�Bt�Bt�Br�BnzBkiBfHB_B_Ba+Bc7Bd;B^B_B]B`$B_B_B^B_B_B^B]B\B\B\	B[BY�B^B_Ba&Bc4Bc4Be@BfIBfGBiZBljBo�Bq�Bq�Bu�Bv�Bz�B{�B}�B}�B~�B~�B�B��B��B�B�B�$B�$B�%B�$B�iB��B��B��B��B��B�B�B�B�B�B�(B�8B�dB�vBłBǏBɛB̮B��B��B�B�!B�/B�OB�fB�B�B��B��B��B��B	 �B	�B	�B	�B	�B	B	B	'B	+B	4B	VB	TB	hB	wB	zB	�B	 �B	&�B	(�B	.�B	3B	6'B	;EB	AhB	F�B	O�B	T�B	X�B	\B	_B	c4B	d<B	lkB	oB	q�B	s�B	r�B	nzB	j\B	i[B	iXB	i\B	lkB	s�B	t�B	t�B	w�B	x�B	z�B	|�B	|�B	~�B	��B	��B	� B	�B	�$B	�!B	�$B	�7B	�=B	�=B	�DB	�OB	�XB	�bB	�iB	�dB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�(B	�1B	�IB	�]B	�cB	�hB	�hB	�qB	�wB	�wB	�~B	ƉB	ȒB	ɚB	ʢB	ʢB	˦B	ʹB	ʹB	κB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�"B	�%B	�(B	�0B	�-B	�3B	�HB	�DB	�LB	�KB	�TB	�TB	�VB	�_B	�`B	�jB	�pB	�qB	�rB	�pB	�qB	�wB	�wG�O�B	�&B	�pB

�B
�B
$7B
)�B
/}B
8�B
?�B
B�B
KsB
Q}B
T�B
Z�B
^~B
dB
j-B
k�B
r�B
v'B
zXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�B��B�}G�O�G�O�B�jB��G�O�G�O�G�O�G�O�G�O�B
�XG�O�G�O�G�O�B�6B��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�vG�O�G�O�G�O�G�O�G�O�G�O�BMSG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�mBƦG�O�G�O�G�O�B��G�O�G�O�G�O�B�
B�:G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A�ΡG�O�G�O�B�B�?G�O�G�O�G�O�A��PG�O�B
[B��G�O�G�O�B�_G�O�G�O�G�O�G�O�G�O�B��B��G�O�A�3�B��B��B�NG�O�G�O�B��B�B5�B��G�O�B�DG�O�G�O�B��B��G�O�A� �G�O�B��B�?G�O�G�O�G�O�G�O�G�O�G�O�B�XG�O�B	��B��B�
G�O�G�O�A���B�=G�O�B�G�O�B��G�O�B�iG�O�G�O�G�O�B��B
{)BkgG�O�G�O�B��B�CB��B�{G�O�G�O�G�O�BR�G�O�G�O�B��G�O�G�O�B�G�O�G�O�BŉB�G�O�G�O�G�O�B��G�O�G�O�B��B��A�C�G�O�B�{B@B�.B�9B,�G�O�G�O�B�ZG�O�B�OB�tB��G�O�BC�B�B��B�GB��G�O�G�O�B��B��B��B�XB	-8G�O�B�B�B�:BftB�.B��B�cB��G�O�G�O�B��B��B�]B�^B�SB��B�DB��B��B��B��B��B��B��B�B�_B�iB�lB��B��B��B��B��B��B��B��B�1B��B�WB��B��B��B��B�AB�(B�WB��B��B�B�B�DB��B��B��B�1B�8B��B��B�XB�jB��B��B��B��B�lB��B�&B�FB��B��B�_B��B��B�B��B��B��B�B��B�)B��B�6B��B��B��B��B��B��B��B�]B�,B�LB��B�B�B�B��B�]B��B��B��B��B�iB�B�2B��G�O�B��B�SB��B��B�2B��B�rB��B�.B�rB�=B��B�XB�XB�IB�]B��B�XB�lB�_B�XB��B��B�oB��B�)B��B� B��B��B�B�vB��B��Bl�B�^B��B�B�eG�O�B�~B��B��B�B��B��B��B��B��B��B��B�5B�^B��B�{B��B��B�YB�gB��B��B��B��B�B�nB��B�]B��B��B��B��B�RB��B�zB��B��B��B��B��B�mB��B�SB��B�8B�rB��B�rB�%B��B�nB��B�B��B��B��B�]B��B��B��B�dB�dB��B��B�B�`B��B��B��B�]B�B�1B��B��B�(B��B�B�%B��B�%B��B��B�6B�6B��B��B�B��B�1B�`B�B��B�B��B��B�=B�6B��B�dB�6B��B��B�.B�]B�%B��B��B��B�fB��B��B��B�~B�	B��B��B�cB�;B�vB�3B�4B��B�
B�B��B��B�*B�XB�kB�LB��B��B�B�XB��B�kB��B��B�dB��B�mB��B��B��B�B��B��B��B��B�B�mB�TB�JB�B�ZB�QB�B��B��B�B�wB�*B�B�B�B��B��B��B�B��B�B�B�B��B��B��B��B�"B�B�KB��B�B�oB��B�B�B��B�B�MB�~B�[B�B�6B�B�B�B�B�2B�lB�aB�zB�B��B�sB�_B�B�B��B�B�NB�B�LB��B�B�B��B�B�B�hB�B�oB��B�;B�zB�B�vB�2B�zB�B�uB��B��B�B�B�oB�oB��B�B��B��B�B�B�oB�B�VB�KB�B��B��B�B�VB�B�B�9B�DB�aB��B�B�B�FB�9B�B�B�IB�zB�9B�&B�B��B��B�hB�B�OB�B�5B�TB�$B�2B�zB�B��B�|B�lB��B��B�_B�B�B�&B��B�iB�uB�-B�B��B�B�qB�@B�)B�jB�B�B�EB�+B�B��B�B��B�B��B�B��B�B�"B�9B�2B�B�B�B�B��B� B��B�~B�}B�B�B��B�B��B��B��B�B��B��B�B�fB�qB�B� B�B��B��B��B�B��B�vB�B�B�B��B�B�B��B��B��B�B�B�*B�B�B�B�B�B��B��B�	B�B��B�B��B��B�~B�B�B	�zB	�1B	�TB	�B	��B	�3B	�B	�dB	�9B	�EB	�KB	��B	�B	�HB	�tB	�B	�B	�4B	�B	��B	��B	��B	�1B	�~B	�B	�B	�B	�B	�?B	�1B	�CB	��B	�XB	�LB	�B	��B	�B	�B	�,B	��B	�B	�mB	�B	�B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�aB	�DB	�YB	�iB	��B	�aB	�B	�<B	��B	��B	�AB	��B	�B	�B	��B	�ZB	�aB	�B	�7B	�OB	�UB	�B	��B	�nB	�B	��B	�?B	�rB	��B	��B	�B	��B	��B	�B	�aB	��B	�{B	�B	�0B	�eB	�B	�B	� B	��B	��B	�qB	�B	�GB	�B	�>B	�B	�6B	�GB	�9B	�_B	�OB	�cB	�6B	�<B	�B	��B	�B	�B	�~B	�B	�B�B��B�B�B�B��B��B��B��B�"B�B�KB��B�B�oB��B�B�B��B�B�MB�~B�[B�B�6B�B�B�B�B�2B�lB�aB�zB�B��B�sB�_B�B�B��B�B�NB�B�LB��B�B�B��B�B�B�hB�B�oB��B�;B�zB�B�vB�2B�zB�B�uB��B��B�B�B�oB�oB��B�B��B��B�B�B�oB�B�VB�KB�B��B��B�B�VB�B�B�9B�DB�aB��B�B�B�FB�9B�B�B�IB�zB�9B�&B�B��B��B�hB�B�OB�B�5B�TB�$B�2B�zB�B��B�|B�lB��B��B�_B�B�B�&B��B�iB�uB�-B�B��B�B�qB�@B�)B�jB�B�B�EB�+B�B��B�B��B�B��B�B��B�B�"B�9B�2B�B�B�B�B��B� B��B�~B�}B�B�B��B�B��B��B��B�B��B��B�B�fB�qB�B� B�B��B��B��B�B��B�vB�B�B�B��B�B�B��B��B��B�B�B�*B�B�B�B�B�B��B��B�	B�B��B�B��B��B�~B�B�B	�zB	�1B	�TB	�B	��B	�3B	�B	�dB	�9B	�EB	�KB	��B	�B	�HB	�tB	�B	�B	�4B	�B	��B	��B	��B	�1B	�~B	�B	�B	�B	�B	�?B	�1B	�CB	��B	�XB	�LB	�B	��B	�B	�B	�,B	��B	�B	�mB	�B	�B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�aB	�DB	�YB	�iB	��B	�aB	�B	�<B	��B	��B	�AB	��B	�B	�B	��B	�ZB	�aB	�B	�7B	�OB	�UB	�B	��B	�nB	�B	��B	�?B	�rB	��B	��B	�B	��B	��B	�B	�aB	��B	�{B	�B	�0B	�eB	�B	�B	� B	��B	��B	�qB	�B	�GB	�B	�>B	�B	�6B	�GB	�9B	�_B	�OB	�cB	�6B	�<B	�B	��B	�B	�B	�~B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444344443344334444434443334444444434444443444444443344434443344344444344434433444343344344444334333344333343443343433444444343334433434343444333443333444344344344334443443334333334434333433333443333343333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649232020083116492320200831164923202008311649232020083116492320200831164923202008311649232020083116492320200831164923202008311649232020083116492320200831164923AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817142019021918171420190219181714    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817142019021918171420190219181714  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817142019021918171420190219181714  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649232020083116492320200831164923  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                