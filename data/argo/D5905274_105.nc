CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-19T22:01:31Z creation; 2023-04-26T19:24:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � aP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � i8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201119220131  20230426192430  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               i   iAA  AOAO7315_008643_105                 7315_008643_105                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�H�@���@�H�@���11  @�H�q��@�H�q��@1��7��4@1��7��4�e���[B�e���[B11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�@E�@��\@�p�@�G�@�G�@��RA  A   A+�A>�RA^�RA�Q�A���A���A���A��A�  A�Q�A�  B   B(�B  B(�B Q�B(  B0  B8  B?�
BG�
BP  BW�
B_�Bh(�Bp(�Bx(�B�  B�  B�{B�=qB�(�B�{B�{B�{B�{B�{B��B�  B�  B�{B�(�B�(�B�  B��B��B�{B�{B�{B�(�B�(�B�  B�  B�{B�(�B�  B�  B�  B�  C   C��C  C  C��C
  C  C  C  C  C  C
=C
=C
=C��C  C 
=C"  C#��C&  C(
=C*
=C,
=C.
=C0  C2
=C4
=C6
=C8  C:  C<  C=��C?��CA��CD  CF  CG�CI��CL  CN{CP
=CQ�CS��CV  CW��CY��C[��C^  C`
=Cb  Cc�Cf  Ch  Cj  Ck��Cm��Cp{Cr{Ct{Cv
=Cw��Cy�C{��C~  C��C���C���C���C�C�  C�C�  C�  C�  C�  C���C��C�  C�\C�
=C�  C���C���C�  C�C�  C���C�C���C���C�  C���C���C���C���C�C�C�  C�C���C���C���C�  C�  C�C�  C�C�  C�C�  C���C���C���C�  C�
=C�C�  C�  C�  C�  C�C�C�C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C�C�C�C�
=C�
=C�
=C�C�  C���C���C�  C���C��C���C�  C���C�  C���C�C�C�  C�C�C�  C�C�C���C���C�  C�\C�
=C�
=C�C�  C�  C�C�C�C�C�C���C���C�  C�  C�  C�C�  C���C�  C�
=C�  C���C�  C�  C�  D   D �D  D}qD  D� D�D� D  D� D��D� D  D� D�D��D  D�D	  D	�D
D
}qD�D}qD��D��D�qD}qD�qDxRD�qD��D�D� D�D� D�qD}qD  D}qD�qD��D�D}qD  D}qD  D� D  D� D�D��D�D��D�D� D�qD}qD�qD}qD  D��D  D� D D ��D �qD!��D"�D"��D#D#��D$  D$z�D$�qD%��D&D&��D'�D'� D'�qD(}qD)  D)z�D)�RD*� D+�D+� D,  D,� D-  D-� D.�D.� D/  D/��D0D0�D1�D1��D2�D2z�D2��D3xRD3�qD4��D5�D5� D6  D6� D6��D7}qD8�D8��D9D9�D:  D:z�D:��D;� D<D<�D=D=� D=��D>z�D>��D?� D@�D@� DA  DA}qDA�qDB��DC�DC��DC�qDDz�DE  DE�DF  DFz�DF��DG� DH�DH��DH�qDI� DJ  DJ��DK�DK}qDK�qDL�DMDM� DM��DNz�DN�qDO� DP  DP��DQ�DQ}qDQ��DR��DS
=DS��DT  DTz�DT�qDU� DV  DV� DW�DW}qDW��DX� DY  DY}qDZ�DZ��D[  D[��D\  D\� D]�D]� D^�D^� D^�qD_� D`  D`� Da�Da��Db�Db}qDb��Dc}qDd  Dd� Dd�qDe� DfDf�Dg�Dg��Dh�Dh}qDh�qDi��DjDj��Dk�Dk� Dk�qDlxRDl�qDm� Dn  Dnz�Dn�RDoz�Do��Dpz�Dp�qDq� Dq�qDrz�Dr�qDs� Ds�qDtz�Dt�qDu� Dv  Dv��Dw�Dw� Dw�qDx� Dy�Dy��DzDz��D{  D{� D{��D|z�D}  D}��D~  D~��D�D� D�qD�@ D��HD�D��D�AHD��HD�� D�HD�@ D�� D���D�HD�AHD�� D���D���D�>�D�}qD��qD���D�>�D��HD��HD�HD�AHD�� D��qD���D�=qD�~�D�� D�  D�AHD���D���D�HD�>�D�}qD���D�  D�AHD���D���D��D�AHD��HD��qD��qD�=qD�}qD�� D�HD�B�D�� D���D�  D�AHD�~�D��)D���D�AHD�~�D���D�HD�@ D�� D�D�HD�>�D�� D�� D�HD�AHD�~�D���D�HD�AHD�~�D���D��qD�>�D��HD��HD�  D�>�D�~�D���D���D�>�D�~�D��HD�  D�=qD�}qD���D�  D�>�D�� D��HD���D�@ D���D���D�  D�@ D�� D�D�  D�@ D�� D�� D�  D�@ D�� D��qD�  D�AHD�� D��HD��qD�>�D�~�D��qD���D�=qD��HD�� D�HD�@ D��HD�� D���D�AHD�� D���D���D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D�� D�D�  D�>�D�~�D���D�HD�@ D�~�D���D�  D�>�D�� D�� D���D�AHD�~�D���D�HD�AHD���D��HD�  D�AHD��HD���D�  D�@ D�}qD���D���D�>�D�~�D�� D���D�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D��qD�  D�B�D�� D��HD�HD�AHD��HD��HD�  D�@ D�~�D��qD�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�=qD�� D�� D��qD�@ D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�=qD�}qD��qD���D�>�D�~�D�� D�  D�>�D�� D�D�  D�=qD�}qD���D�  D�AHD D�� D�  D�>�D�~�D�� D�HD�=qDĀ D��HD�  D�AHD�~�DŽqD�  D�C�DƁHDƾ�D��)D�=qD�~�DǽqD���D�@ DȀ DȾ�D��)D�>�Dɀ Dɾ�D���D�=qD�}qDʼ)D��)D�=qDˀ D��HD�  D�>�D̀ D�� D�  D�@ D̀ D�� D��qD�<)D�|)DνqD���D�>�Dπ D�� D�HD�B�DЁHD�D��D�AHDр DѾ�D�  D�>�DҁHD�� D�HD�AHDӀ DӽqD���D�>�D�}qD�� D�HD�AHDՀ Dվ�D���D�>�D�~�DֽqD��qD�=qD�~�D�� D��D�B�D؂�D��HD���D�@ D�~�D��HD��qD�@ Dڀ D�� D���D�=qD�~�D��HD��D�@ D܀ D�D�HD�AHD݂�D�D��D�B�Dހ D޾�D�  D�B�D�~�D��HD�HD�=qD�~�DྸD��qD�>�D�~�D�qD���D�B�D�HD�� D�HD�@ D� D��HD�  D�@ D� D侸D��D�@ D� D�� D��D�B�D�HD�D�HD�AHD炏D�� D���D�>�D�~�D辸D���D�@ D�HD龸D�  D�AHD�HD�� D���D�>�D� D�D���D�>�D� D�� D�HD�AHD�HD�� D�  D�AHD�~�D�� D�  D�AHD� D�� D���D�>�D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�HD�@ D�~�D�� D���D�=qD�}qD���D��qD�=qD�� D�� D���D�>�D�~�D��HD��D�@ D�~�D��qD�  D�@ D�� D��HD�HD�=qD�� D�� D���D�AHD�~�D�� D��D�,�>�G�>�?.{?�\)?�p�?�G�@�@�R@0��@E�@^�R@z�H@�=q@�z�@��\@��@�  @���@ٙ�@�ff@�33@�p�A�A�A�AQ�A�RA%A+�A2�\A8��A?\)AEAL��AS33AX��A_\)AfffAl��Ar�\Ay��A�  A��
A�
=A�=qA�A���A�(�A�
=A�=qA�p�A���A��A�
=A�=qA��A�Q�A��A��RA��A��A�  A�33A�A�Q�A˅AθRAљ�A���A�\)Aڏ\A�p�A�Q�A�\A�p�A�Q�A�\A��A�
=A���A�33A��A��RA���A�=qA�(�A�B   B ��BB�RB�Bz�B��B�RB�
B��B	B
�RB�B��BB�\B\)BQ�BG�B=qB\)Bz�BG�BffB\)BQ�BG�BffB�B��B�B�HB�B ��B!B"�RB#�B$��B%��B&ffB'\)B(Q�B)G�B*=qB+�B,z�B-p�B.ffB/�B0��B2{B3
=B4  B4��B5�B7
=B8  B8��B9�B;
=B;�
B<��B=B>ffB?�B@z�BAp�BBffBC\)BDQ�BEG�BF=qBG33BH(�BIG�BJffBK\)BL(�BM�BN=qBO33BP  BP��BQ�BR�RBS�BTQ�BU�BV{BV�HBW�
BX��BZ{B[33B\(�B\��B^{B^�HB`  B`��BaBb�\Bc�BdQ�BeG�Bf=qBg33Bh(�Bi�Bj{Bk
=Bk�
Bl��Bm�Bn�RBo�Bp��Bq�Br�HBs�
Bt��Bu��Bv�\Bw�Bx��By��Bz�\B{�B|��B}��B~�\B�B�=qB���B�G�B��
B�Q�B���B�\)B�  B�z�B�
=B��B�  B�z�B���B�p�B�  B�z�B���B�p�B�  B�ffB���B�p�B�  B�ffB���B�p�B��B�ffB���B�\)B��
B�Q�B��HB�\)B��
B�(�B���B�33B��B�(�B���B��B�p�B��B�Q�B���B�G�B�B�{B���B���B�\)B��
B�Q�B��RB�33B���B�{B�z�B���B�G�B�B�=qB��RB��B���B�  B��\B�
=B��B��B�z�B���B��B�  B��\B��B��B�(�B���B�G�B��
B�z�B�
=B��B�=qB��HB�\)B�  B��\B��B��B�=qB���B�p�B��B��\B�
=B���B�{B��\B��B��B�=qB���B�\)B��B�z�B�
=B���B�=qB���B�p�B�  B���B�G�B��
B�z�B��B�B�=qB��HB��B�(�B���B�G�B�B�ffB�
=BÅB�{BĸRB�G�B�{BƸRB�p�B�  Bȣ�B�G�B��Bʏ\B�33B��
B̏\B�
=B�B�Q�B��HBυB�(�B���B�p�B�{BҸRB�\)B�  Bԏ\B�33B�  B֣�B�\)B�  Bأ�B�G�B��Bڏ\B���Bۙ�B�=qB��HB�\)B�  B���B�\)B�(�B�RB�G�B�B�ffB��HB�B�Q�B�
=B噚B�=qB�RB�33B�B�(�B��HB�\)B�B�(�B�z�B�RB�
=B��B�\)B�B�B�{B�ffB�\B��HB�
=B�33B�G�B�p�B��B��
B�{B�ffB��B���B�33B�p�BBB��
B�{B�Q�B�z�B�RB���B�p�B�B��
B�{B�Q�B�\B�RB���B�
=B�G�B�p�B�B�{B�ffB�\B���B�
=B�G�B��B��B�B��B�(�B�Q�B��RB�
=B�33B�p�B��B�B��
B�{B�=qB�z�B��RB�
=B�\)B��B��
B�  B�{B�Q�B�ffB���B��HB�
=B��B��B��B�{B�=qB�z�B��\B���B�\)B���B��B�  B�=qB�ffB��RB��B�p�B�B��C 
=C (�C Q�C �C ��C �
C �HC  C�CG�Cp�C�C��C��C�C�CQ�CffC�C�
C��C  C(�CQ�C�\C�RC�C�C{C33CffC��C��C�C��C�C=qCz�C�C�HC��C{C=qCp�C�C�
C  C{C=qCffC�C�
C
=C(�C=qCp�C��C�
C	
=C	=qC	\)C	z�C	��C	�C
�C
Q�C
p�C
�\C
C  C=qCp�C��C�RC�HC(�CffC��C�C�HC(�C\)C��CC�
C{C=qC�CC�C(�C=qCp�C��C�C�CG�Cp�C��CC
=CG�C�C�\CC�C33CffC��C�RC�HC{C\)C�\C��C��C
=C(�Cz�C��C�
C�C{C\)C��CC�HC
=CQ�C�C�RC��C  CG�Cz�C�CC��C�C\)C��C�
C��C�CG�C��C��C�
C  CG�Cz�C��CC�C33C\)C�\C��C�
C�CG�CffC�\C�
C{C�C\)C��C�
C��C�Cz�C�RCC  CG�Cz�C�C��C 
=C \)C �\C �C �
C!33C!p�C!�C!�RC"
=C"=qC"p�C"��C"�
C#{C#Q�C#\)C#�\C#�HC${C$(�C$ffC$��C$�HC%  C%(�C%ffC%�C%C&  C&Q�C&�C&��C&�C'(�C'=qC'p�C'��C(
=C(�C(\)C(��C(��C(��C)G�C)z�C)��C)C*�C*Q�C*ffC*��C*�C+(�C+=qC+p�C+C,  C,{C,Q�C,��C,C,�C-�C-p�C-��C-C.  C.G�C.p�C.��C.�HC/�C/G�C/p�C/C0  C0{C0G�C0�\C0��C0��C1�C1z�C1�C1��C2  C2\)C2��C2�RC3  C3G�C3p�C3��C3��C4=qC4Q�C4�\C4�C5�C5G�C5��C5�HC6
=C6G�C6��C6��C7  C7ffC7��C7C8{C8\)C8�C8��C9�C9G�C9z�C9��C9��C:33C:�C:�RC:�C;G�C;ffC;��C<  C<(�C<\)C<C<�HC=�C=p�C=��C=�
C>33C>Q�C>��C?  C?�C?p�C?C?�HC@(�C@�\C@�RCA
=CAffCA�CA�
CB(�CBQ�CB�CB��CC�CC�CC�RCC�CDQ�CD�CDCE(�CE\)CE�CF
=CF33CF��CF�HCG
=CGffCG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        ?�=q@�@E�@��\@�p�@�G�@�G�@��RA  A   A+�A>�RA^�RA�Q�A���A���A���A��A�  A�Q�A�  B   B(�B  B(�B Q�B(  B0  B8  B?�
BG�
BP  BW�
B_�Bh(�Bp(�Bx(�B�  B�  B�{B�=qB�(�B�{B�{B�{B�{B�{B��B�  B�  B�{B�(�B�(�B�  B��B��B�{B�{B�{B�(�B�(�B�  B�  B�{B�(�B�  B�  B�  B�  C   C��C  C  C��C
  C  C  C  C  C  C
=C
=C
=C��C  C 
=C"  C#��C&  C(
=C*
=C,
=C.
=C0  C2
=C4
=C6
=C8  C:  C<  C=��C?��CA��CD  CF  CG�CI��CL  CN{CP
=CQ�CS��CV  CW��CY��C[��C^  C`
=Cb  Cc�Cf  Ch  Cj  Ck��Cm��Cp{Cr{Ct{Cv
=Cw��Cy�C{��C~  C��C���C���C���C�C�  C�C�  C�  C�  C�  C���C��C�  C�\C�
=C�  C���C���C�  C�C�  C���C�C���C���C�  C���C���C���C���C�C�C�  C�C���C���C���C�  C�  C�C�  C�C�  C�C�  C���C���C���C�  C�
=C�C�  C�  C�  C�  C�C�C�C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C�C�C�C�
=C�
=C�
=C�C�  C���C���C�  C���C��C���C�  C���C�  C���C�C�C�  C�C�C�  C�C�C���C���C�  C�\C�
=C�
=C�C�  C�  C�C�C�C�C�C���C���C�  C�  C�  C�C�  C���C�  C�
=C�  C���C�  C�  C�  D   D �D  D}qD  D� D�D� D  D� D��D� D  D� D�D��D  D�D	  D	�D
D
}qD�D}qD��D��D�qD}qD�qDxRD�qD��D�D� D�D� D�qD}qD  D}qD�qD��D�D}qD  D}qD  D� D  D� D�D��D�D��D�D� D�qD}qD�qD}qD  D��D  D� D D ��D �qD!��D"�D"��D#D#��D$  D$z�D$�qD%��D&D&��D'�D'� D'�qD(}qD)  D)z�D)�RD*� D+�D+� D,  D,� D-  D-� D.�D.� D/  D/��D0D0�D1�D1��D2�D2z�D2��D3xRD3�qD4��D5�D5� D6  D6� D6��D7}qD8�D8��D9D9�D:  D:z�D:��D;� D<D<�D=D=� D=��D>z�D>��D?� D@�D@� DA  DA}qDA�qDB��DC�DC��DC�qDDz�DE  DE�DF  DFz�DF��DG� DH�DH��DH�qDI� DJ  DJ��DK�DK}qDK�qDL�DMDM� DM��DNz�DN�qDO� DP  DP��DQ�DQ}qDQ��DR��DS
=DS��DT  DTz�DT�qDU� DV  DV� DW�DW}qDW��DX� DY  DY}qDZ�DZ��D[  D[��D\  D\� D]�D]� D^�D^� D^�qD_� D`  D`� Da�Da��Db�Db}qDb��Dc}qDd  Dd� Dd�qDe� DfDf�Dg�Dg��Dh�Dh}qDh�qDi��DjDj��Dk�Dk� Dk�qDlxRDl�qDm� Dn  Dnz�Dn�RDoz�Do��Dpz�Dp�qDq� Dq�qDrz�Dr�qDs� Ds�qDtz�Dt�qDu� Dv  Dv��Dw�Dw� Dw�qDx� Dy�Dy��DzDz��D{  D{� D{��D|z�D}  D}��D~  D~��D�D� D�qD�@ D��HD�D��D�AHD��HD�� D�HD�@ D�� D���D�HD�AHD�� D���D���D�>�D�}qD��qD���D�>�D��HD��HD�HD�AHD�� D��qD���D�=qD�~�D�� D�  D�AHD���D���D�HD�>�D�}qD���D�  D�AHD���D���D��D�AHD��HD��qD��qD�=qD�}qD�� D�HD�B�D�� D���D�  D�AHD�~�D��)D���D�AHD�~�D���D�HD�@ D�� D�D�HD�>�D�� D�� D�HD�AHD�~�D���D�HD�AHD�~�D���D��qD�>�D��HD��HD�  D�>�D�~�D���D���D�>�D�~�D��HD�  D�=qD�}qD���D�  D�>�D�� D��HD���D�@ D���D���D�  D�@ D�� D�D�  D�@ D�� D�� D�  D�@ D�� D��qD�  D�AHD�� D��HD��qD�>�D�~�D��qD���D�=qD��HD�� D�HD�@ D��HD�� D���D�AHD�� D���D���D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D�� D�D�  D�>�D�~�D���D�HD�@ D�~�D���D�  D�>�D�� D�� D���D�AHD�~�D���D�HD�AHD���D��HD�  D�AHD��HD���D�  D�@ D�}qD���D���D�>�D�~�D�� D���D�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D��qD�  D�B�D�� D��HD�HD�AHD��HD��HD�  D�@ D�~�D��qD�HD�@ D�~�D�� D���D�>�D�~�D�� D�  D�=qD�� D�� D��qD�@ D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�=qD�}qD��qD���D�>�D�~�D�� D�  D�>�D�� D�D�  D�=qD�}qD���D�  D�AHD D�� D�  D�>�D�~�D�� D�HD�=qDĀ D��HD�  D�AHD�~�DŽqD�  D�C�DƁHDƾ�D��)D�=qD�~�DǽqD���D�@ DȀ DȾ�D��)D�>�Dɀ Dɾ�D���D�=qD�}qDʼ)D��)D�=qDˀ D��HD�  D�>�D̀ D�� D�  D�@ D̀ D�� D��qD�<)D�|)DνqD���D�>�Dπ D�� D�HD�B�DЁHD�D��D�AHDр DѾ�D�  D�>�DҁHD�� D�HD�AHDӀ DӽqD���D�>�D�}qD�� D�HD�AHDՀ Dվ�D���D�>�D�~�DֽqD��qD�=qD�~�D�� D��D�B�D؂�D��HD���D�@ D�~�D��HD��qD�@ Dڀ D�� D���D�=qD�~�D��HD��D�@ D܀ D�D�HD�AHD݂�D�D��D�B�Dހ D޾�D�  D�B�D�~�D��HD�HD�=qD�~�DྸD��qD�>�D�~�D�qD���D�B�D�HD�� D�HD�@ D� D��HD�  D�@ D� D侸D��D�@ D� D�� D��D�B�D�HD�D�HD�AHD炏D�� D���D�>�D�~�D辸D���D�@ D�HD龸D�  D�AHD�HD�� D���D�>�D� D�D���D�>�D� D�� D�HD�AHD�HD�� D�  D�AHD�~�D�� D�  D�AHD� D�� D���D�>�D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�HD�@ D�~�D�� D���D�=qD�}qD���D��qD�=qD�� D�� D���D�>�D�~�D��HD��D�@ D�~�D��qD�  D�@ D�� D��HD�HD�=qD�� D�� D���D�AHD�~�D�� D��G�O�>�G�>�?.{?�\)?�p�?�G�@�@�R@0��@E�@^�R@z�H@�=q@�z�@��\@��@�  @���@ٙ�@�ff@�33@�p�A�A�A�AQ�A�RA%A+�A2�\A8��A?\)AEAL��AS33AX��A_\)AfffAl��Ar�\Ay��A�  A��
A�
=A�=qA�A���A�(�A�
=A�=qA�p�A���A��A�
=A�=qA��A�Q�A��A��RA��A��A�  A�33A�A�Q�A˅AθRAљ�A���A�\)Aڏ\A�p�A�Q�A�\A�p�A�Q�A�\A��A�
=A���A�33A��A��RA���A�=qA�(�A�B   B ��BB�RB�Bz�B��B�RB�
B��B	B
�RB�B��BB�\B\)BQ�BG�B=qB\)Bz�BG�BffB\)BQ�BG�BffB�B��B�B�HB�B ��B!B"�RB#�B$��B%��B&ffB'\)B(Q�B)G�B*=qB+�B,z�B-p�B.ffB/�B0��B2{B3
=B4  B4��B5�B7
=B8  B8��B9�B;
=B;�
B<��B=B>ffB?�B@z�BAp�BBffBC\)BDQ�BEG�BF=qBG33BH(�BIG�BJffBK\)BL(�BM�BN=qBO33BP  BP��BQ�BR�RBS�BTQ�BU�BV{BV�HBW�
BX��BZ{B[33B\(�B\��B^{B^�HB`  B`��BaBb�\Bc�BdQ�BeG�Bf=qBg33Bh(�Bi�Bj{Bk
=Bk�
Bl��Bm�Bn�RBo�Bp��Bq�Br�HBs�
Bt��Bu��Bv�\Bw�Bx��By��Bz�\B{�B|��B}��B~�\B�B�=qB���B�G�B��
B�Q�B���B�\)B�  B�z�B�
=B��B�  B�z�B���B�p�B�  B�z�B���B�p�B�  B�ffB���B�p�B�  B�ffB���B�p�B��B�ffB���B�\)B��
B�Q�B��HB�\)B��
B�(�B���B�33B��B�(�B���B��B�p�B��B�Q�B���B�G�B�B�{B���B���B�\)B��
B�Q�B��RB�33B���B�{B�z�B���B�G�B�B�=qB��RB��B���B�  B��\B�
=B��B��B�z�B���B��B�  B��\B��B��B�(�B���B�G�B��
B�z�B�
=B��B�=qB��HB�\)B�  B��\B��B��B�=qB���B�p�B��B��\B�
=B���B�{B��\B��B��B�=qB���B�\)B��B�z�B�
=B���B�=qB���B�p�B�  B���B�G�B��
B�z�B��B�B�=qB��HB��B�(�B���B�G�B�B�ffB�
=BÅB�{BĸRB�G�B�{BƸRB�p�B�  Bȣ�B�G�B��Bʏ\B�33B��
B̏\B�
=B�B�Q�B��HBυB�(�B���B�p�B�{BҸRB�\)B�  Bԏ\B�33B�  B֣�B�\)B�  Bأ�B�G�B��Bڏ\B���Bۙ�B�=qB��HB�\)B�  B���B�\)B�(�B�RB�G�B�B�ffB��HB�B�Q�B�
=B噚B�=qB�RB�33B�B�(�B��HB�\)B�B�(�B�z�B�RB�
=B��B�\)B�B�B�{B�ffB�\B��HB�
=B�33B�G�B�p�B��B��
B�{B�ffB��B���B�33B�p�BBB��
B�{B�Q�B�z�B�RB���B�p�B�B��
B�{B�Q�B�\B�RB���B�
=B�G�B�p�B�B�{B�ffB�\B���B�
=B�G�B��B��B�B��B�(�B�Q�B��RB�
=B�33B�p�B��B�B��
B�{B�=qB�z�B��RB�
=B�\)B��B��
B�  B�{B�Q�B�ffB���B��HB�
=B��B��B��B�{B�=qB�z�B��\B���B�\)B���B��B�  B�=qB�ffB��RB��B�p�B�B��C 
=C (�C Q�C �C ��C �
C �HC  C�CG�Cp�C�C��C��C�C�CQ�CffC�C�
C��C  C(�CQ�C�\C�RC�C�C{C33CffC��C��C�C��C�C=qCz�C�C�HC��C{C=qCp�C�C�
C  C{C=qCffC�C�
C
=C(�C=qCp�C��C�
C	
=C	=qC	\)C	z�C	��C	�C
�C
Q�C
p�C
�\C
C  C=qCp�C��C�RC�HC(�CffC��C�C�HC(�C\)C��CC�
C{C=qC�CC�C(�C=qCp�C��C�C�CG�Cp�C��CC
=CG�C�C�\CC�C33CffC��C�RC�HC{C\)C�\C��C��C
=C(�Cz�C��C�
C�C{C\)C��CC�HC
=CQ�C�C�RC��C  CG�Cz�C�CC��C�C\)C��C�
C��C�CG�C��C��C�
C  CG�Cz�C��CC�C33C\)C�\C��C�
C�CG�CffC�\C�
C{C�C\)C��C�
C��C�Cz�C�RCC  CG�Cz�C�C��C 
=C \)C �\C �C �
C!33C!p�C!�C!�RC"
=C"=qC"p�C"��C"�
C#{C#Q�C#\)C#�\C#�HC${C$(�C$ffC$��C$�HC%  C%(�C%ffC%�C%C&  C&Q�C&�C&��C&�C'(�C'=qC'p�C'��C(
=C(�C(\)C(��C(��C(��C)G�C)z�C)��C)C*�C*Q�C*ffC*��C*�C+(�C+=qC+p�C+C,  C,{C,Q�C,��C,C,�C-�C-p�C-��C-C.  C.G�C.p�C.��C.�HC/�C/G�C/p�C/C0  C0{C0G�C0�\C0��C0��C1�C1z�C1�C1��C2  C2\)C2��C2�RC3  C3G�C3p�C3��C3��C4=qC4Q�C4�\C4�C5�C5G�C5��C5�HC6
=C6G�C6��C6��C7  C7ffC7��C7C8{C8\)C8�C8��C9�C9G�C9z�C9��C9��C:33C:�C:�RC:�C;G�C;ffC;��C<  C<(�C<\)C<C<�HC=�C=p�C=��C=�
C>33C>Q�C>��C?  C?�C?p�C?C?�HC@(�C@�\C@�RCA
=CAffCA�CA�
CB(�CBQ�CB�CB��CC�CC�CC�RCC�CDQ�CD�CDCE(�CE\)CE�CF
=CF33CF��CF�HCG
=CGffCG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�_@�LG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�v�A�v�A�v�A�x�A�~�A�~�A݁A݃A݅A݁A݃A݇+A݉7A݇+A݉7A݇+A݇+A݉7A݋DAݍPAݏ\Aݏ\A݉7AݍPAݏ\Aݕ�AݓuAݏ\AݓuAݛ�Aݩ�A�ĜA�/A� �Aݏ\A��#Aܧ�Aܙ�A܁A�G�A�$�A��`A�A�A�dZA�C�A�Aӝ�A�(�A�?}A�ƨAϼjA�&�A΁A��HA̙�A�~�A�Q�A���A�A�jA�9XAŰ!A��A�r�A�|�A�ĜA�&�A�  A�Q�A�p�A��;A�5?A�=qA��+A�A�A���A�t�A�I�A�ffA�O�A�1'A�A�ZA�n�A���A���A�bNA���A�dZA�?}A���A�7LA�v�A��A�;dA���A���A���A��-A���A�
=A�^5A�?}A�ZA��#A��-A�ĜA���A���A�A�A��A��HA���A��!A�p�A�oA��Az�Ay�Ay%AxI�AwK�At�uAq�wAo�Anr�Am"�AkAhjAd�RAc
=Ab�A_VA[��AY�
AW�#AWAVI�ATn�ASO�ARE�AO��AK�AFA�AC\)A@�RA<�A8r�A6�yA4-A2�yA1/A01'A-�PA,A�A+��A)�A&�yA%dZA$�A#�PA#XA!��A!7LA ��A bNA=qA{At�A?}A=qA��AC�A��A �A�A�^A�yAZA^5AZAQ�A\)A{A~�A�HA%AA�A�A��A�;A��A+AQ�A��AG�A
�A	��A�RAbNA=qA=qA=qAAz�A\)AȴAE�AJA�mA��Ap�A33A��A��A�RA��Av�AI�A�A�A ��A �!A �@���@�@��j@�bN@�I�@�1'@���@��\@��@���@��@���@��h@�`B@�V@�;d@�?}@�p�@�9X@��@�F@�+@�^@�?}@���@���@��@�%@�u@��;@���@�^5@�=q@���@���@�^@�Ĝ@�Z@�1@ߍP@�33@�o@���@���@��y@���@ާ�@�v�@�^5@��T@�p�@�O�@�V@���@ܴ9@ܣ�@܋D@�I�@�(�@��;@�dZ@�M�@ش9@և+@�-@ղ-@�X@��@��/@�Ĝ@ԋD@�A�@�\)@҇+@��#@ѡ�@�`B@���@�bN@Ͼw@ϝ�@�|�@�S�@�+@���@���@͑h@̴9@�r�@�A�@�b@�ƨ@˕�@�;d@���@�n�@�5?@�{@��@�/@�Z@�t�@�@���@Ƨ�@�v�@�=q@�`B@Ĵ9@�Z@�o@��@���@�X@��@���@�Z@�l�@��@���@���@���@�{@���@���@��9@��u@�1@�;d@�
=@���@�^5@�$�@��@���@�hs@��@�z�@�j@�Q�@�I�@�1'@��@���@��T@��@�/@���@��/@�Ĝ@��9@���@�Z@�  @��P@�K�@��@�ȴ@���@���@��+@�ff@�J@��@�&�@�Ĝ@�1'@�K�@��R@��\@�v�@�E�@�@���@�O�@��D@��
@���@�5?@�O�@�I�@�K�@��@�E�@���@�7L@���@��j@��@�1'@�b@�K�@�~�@�ff@�V@�=q@��@��@��h@�`B@�&�@��@��@���@��@�1'@� �@��;@��H@�E�@�@�p�@�G�@�%@��@�j@�b@��m@��w@��P@��@�o@�
=@��y@���@�ȴ@���@�^5@�=q@��-@�O�@�G�@�7L@�7L@��@���@��9@���@�z�@�Z@�(�@�1@��@���@��P@���@���@�V@���@�G�@�%@���@���@�j@�9X@��@��F@�t�@�;d@��@�@��@���@��R@��!@���@���@��\@��+@�~�@�v�@�ff@�ff@�V@�$�@���@�G�@�Ĝ@�Q�@� �@���@���@���@��@�K�@��@��@�M�@��T@���@���@�`B@�?}@��@���@�r�@�j@�Q�@�b@��F@�|�@�+@���@�-@��#@��^@��-@���@���@��h@�x�@�p�@�p�@�X@�&�@��@��u@�I�@��
@��@��@�l�@�K�@�;d@�+@�@�ȴ@��!@�n�@�@��#@���@���@��^@���@��7@�p�@�`B@�7L@�V@��@�Ĝ@�r�@�A�@�(�@��@�b@�1@���@���@��m@��;@�|�@�+@���@�v�@�~�@�n�@�n�@�E�@�{@���@��T@��T@���@���@��^@��-@�p�@���@���@��@���@���@��@�z�@�r�@�Z@�A�@�(�@�1@�@�w@�P@~��@~��@~{@}�@|��@|�D@|Z@|�@{ƨ@{@z�H@z��@zM�@yhs@x�@w�w@w�P@w|�@v�R@vE�@v$�@u�@u�h@u?}@u/@u�@t�j@s��@r��@r=q@r-@q�#@q&�@p�9@p �@o|�@m/@l�j@l��@lj@lZ@lI�@kƨ@k33@j�@j-@i�#@i�#@i��@iX@hb@g�w@g|�@g+@g
=@g
=@f��@f�R@f��@f�+@dI�@bn�@a��@a�#@a��@a��@ax�@aX@a&�@`��@`Ĝ@_l�@^�+@^5?@]��@]�@]/@\�/@\�j@\�j@\j@[�F@Z�H@ZM�@Z�@Y�@Y7L@XĜ@W��@W+@V�@Vȴ@Vff@U@U��@UV@T��@TI�@S��@S��@St�@SC�@So@R�@R~�@Q��@P �@O�@O�P@Ol�@OK�@O+@N��@Nȴ@Nv�@M�@M�-@L�@Lz�@L�@K"�@J�\@J^5@J^5@JM�@J=q@I��@Ix�@I&�@H��@H�@HQ�@H1'@H1'@H1'@Hb@H  @G�;@G�@G�P@G|�@G;d@F��@F��@F$�@E�-@EV@Dz�@C�
@C��@CS�@C"�@C@B�@B��@B��@B~�@Bn�@Bn�@Bn�@B^5@B-@A��@A�7@Ahs@A&�@A%@A%@@�`@@Ĝ@@��@@��@@��@@��@@��@@�u@@�@@�@@�@@ �@@  @?�@?l�@?\)@?+@>�y@>�R@>v�@>5?@=�-@=p�@=`B@=O�@=�@<��@<I�@;��@;�m@;��@;t�@;C�@;"�@;o@;o@;@:��@:M�@9��@9hs@9X@97L@8��@8r�@8b@7�w@7�@7��@7+@7�@7
=@6��@6�y@6�y@6ȴ@6��@6V@5��@5O�@4�/@4Z@4�@3�m@3��@3@2��@2��@2�\@2M�@1x�@1%@0�9@0�u@0 �@/\)@.ȴ@.��@.��@.��@.��@.��@.ff@-@,�D@+�
@*��@)x�@)X@)&�@(Ĝ@(��@(�9@(�@'�@'�;@'�w@'��@'\)@'K�@'
=@&ff@%�@%�-@%�h@%O�@%V@$��@$Z@#�m@#��@#S�@#"�@#o@"��@"��@"J@!��@!��@!�^@!��@!�^@!��@!�7@!hs@!G�@!%@ ��@ Ĝ@ 1'@   @��@�w@�@�@�P@|�@l�@;d@
=@ȴ@v�@��@�@��@1@ƨ@�F@��@��@�@dZ@dZ@C�@33@33@o@�@�H@�!@M�@=q@-@-@=q@-@-@-@�@�@J@��@�#@7L@Ĝ@bN@�@�@��@��@�P@l�@�@��@��@��@�y@�@ff@$�@@�T@@@`B@V@�@z�@Z@I�@��@��@S�@33@@�\@M�@�@�@��@��@x�@x�@7L@��@��@�9@r�@A�@1'@ �@  @�w@�P@|�@l�@+@��@�@�R@�R@��@�+A�x�A�z�A�x�A�z�A�t�A�v�A�v�A�t�A�v�A�x�A�z�A�z�A�z�A�x�A�z�A�|�A�|�A�z�A�~�A�|�A݁A݃A݃A݅A݇+A݉7A݅A݃A݃A݅A݅A݅A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݇+A݇+A݅A݅A݅A݅A݅A݇+A݇+A݇+A݇+A݇+A݅A݇+A݅A݇+A݅A݇+A݅A݅A݅A݅A݇+A݉7A݋DA݋DAݍPAݍPAݍPAݍPAݍPAݍPAݍPA݉7A݋DAݍPA݋DA݋DA݋DAݍPAݍPAݏ\Aݏ\Aݏ\AݍPAݑhAݑhAݏ\Aݏ\AݑhAݓuAݑhAݏ\Aݏ\AݍPAݍPAݍPAݍPAݏ\AݍPA݋DA݋DA݋DA݋DA݋DA݋DA݋DA݉7A݉7AݍPA݋DAݍPAݑhAݏ\Aݏ\A݋DA݋DA݋DA݇+AݍPAݍPAݓuAݗ�AݓuAݕ�Aݗ�Aݗ�Aݙ�Aݙ�Aݛ�Aݛ�Aݙ�Aݗ�AݑhAݑhAݍPAݍPAݍPA݋DA݉7Aݏ\AݍPAݓuAݑhAݑhAݍPAݓuAݕ�Aݕ�Aݕ�Aݏ\AݑhAݙ�Aݛ�Aݗ�Aݗ�AݓuAݗ�Aݟ�Aݡ�Aݝ�Aݝ�Aݝ�Aݥ�Aݣ�Aݝ�Aݧ�Aݟ�Aݥ�Aݣ�AݮAݲ-AݶFAݺ^Aݴ9AݮAݴ9AݾwA��#A��TA�A� �A�+A�/A�33A�5?A�;dA�=qA�=qA�=qA�;dA�1'A�+A�$�A��A�VA�A���A��A��HA�ȴAݓuA�hsA�I�A�9XA�-A��A���A��HA���A���AܾwAܴ9Aܰ!Aܩ�Aܧ�Aܩ�Aܧ�Aܧ�Aܧ�Aܧ�Aܡ�Aܛ�Aܙ�Aܗ�Aܗ�Aܗ�Aܕ�Aܕ�AܓuA܏\A܅A܃A�~�A�|�A�x�A�r�A�r�A�bNA�ZA�^5A�M�A�E�A�9XA�/A�+A�$�A�$�A�+A�(�A�+A�$�A��A��A��A�oA�bA�VA�A��A۩�A۝�Aۣ�AۑhA�v�A�jA�\)A�Q�A�C�A�{A��`A��
A�AڮAڛ�AڃA�jA�S�A�7LA�%A���AمA�;dA��A���Aؕ�A�5?AדuA�`BA�;dA��HA�z�A���Aԏ\A�33A��TA�ȴA���AӴ9AӲ-AӬAӣ�AӉ7A�hsA�A�A���A�ffA�33A���Aї�Aч+A�r�A�dZA�dZA�\)A�C�A�{A��mA��HA��#A��A���A���A���Aа!A�l�A� �A���A�ƨAϥ�AϋDA�~�A�v�A�r�A�bNA�M�A�oA��A���AμjAΕ�A΋DA΅A�jA�`BA�Q�A�5?A�-A��A��A͝�A�K�A�A���A�|�A�ZA�|�Ȁ\A̴9A̩�Aˡ�A�`BA�1'A��A���Aʴ9Aʧ�AʃA�;dA��A��A��yA���A�z�A��mA�O�A��
AǑhA�t�A�9XA�1A��AƶFAƥ�AƇ+AƇ+A�v�A�dZA�\)A�M�A�I�A�K�A�K�A�=qA�5?A�$�A�JA�VA���A��`A�ĜAŃA�A��#AĬA�I�A���A���Aç�A�t�A�33A��A��HA���A�ȴA�A�A�AA�ffA�S�A�C�A�;dA�7LA�33A�-A�(�A�"�A��A��A�bA�1A���A��A��;A��
A��FA��A�`BA�M�A�33A�"�A���A��A��HA���A��RA���A�^5A�-A��A��/A��A���A�A��-A���A��uA��+A�|�A�t�A�jA�\)A�VA�Q�A�O�A�K�A�A�A�=qA�7LA�7LA�-A�1'A�$�A��A��A� �A� �A��A�bA�A���A��A��`A��^A���A�z�A�x�A�ffA�XA�33A�  A��;A���A��FA���A��7A�t�A�bNA�Q�A�33A�bA��jA�bNA���A�"�A���A���A�ƨA���A��^A��9A��A���A���A���A���A��7A�~�A�~�A�x�A�p�A�l�A�jA�dZA�`BA�O�A�?}A�33A�/A�1'A�+A��A���A��A��A��9A��9A��!A���A���A���A��hA��A�z�A�t�A�p�A�VA�;dA�+A��A���A���A��A��mA���A��\A�ffA�S�A�K�A�;dA��A�bA���A���A��A��;A��A�A��RA���A��hA��A�jA�\)A�ZA�9XA��A��/A��RA���A��+A�Q�A��A��yA��;A�ȴA�t�A�M�A�/A� �A��A�{A�A�A�`BA�1'A�VA�  A��yA���A��FA���A�|�A�ZA�+A���A��;A���A��A�l�A�l�A�ffA�XA�C�A��A�oA�A���A��A���A��9A��A���A�v�A�G�A�bA��TA��;A��A�A��A�^5A��A��;A��7A��A�%A��;A��
A���A���A��RA��A�x�A�?}A�oA��HA��A��hA��A��A�z�A�jA�33A�(�A�{A�  A��
A���A�jA��A��HA���A�ƨA���A�x�A�I�A�1'A���A��mA���A�O�A�S�A�S�A�;dA�
=A�A�K�A���A�=qA�(�A��A�VA�JA�
=A�JA���A���A��+A�r�A�^5A��A���A��;A��
A�ƨA��FA��!A��-A���A���A�|�A�n�A�ffA�^5A�ZA�XA�VA�S�A�E�A�=qA�-A�$�A��A� �A���A���A��PA�7LA��HA�jA�1'A�VA��#A�`BA��A��HA��A�A��!A���A��\A��\A��hA��A�VA�-A��A�\)A�G�A�33A�(�A�bA�1A���A��A���A��A���A��A�-A���A���A�I�A��!A�l�A�=qA�"�A�  A���A�%A�1A��RA�~�A�5?A�A��A���A�S�A�VA�`BA�l�A�A�A�5?A�oA�`BA�G�A�A�A�A�S�A�-A���A���A��jA��7A��A� �A��A���A��hA�+A��A�&�A��A�JA�
=A�%A���A��A��#A���A���A�ȴA���A�A���A���A���A�ZA���A���A�5?A��A��A�t�A�/A�{A��/A��jA�r�A�E�A� �A��;A���A�t�A���A�/A�A�?}A�ƨA��A�bNA�C�A��A��A��A���A�M�A�A��FA�S�A��A��A�S�A�-A�
=A��A���A��FA���A��PA�\)A�+A��A��A��`A��;A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        A�z�A�v�A�v�A�v�A�x�A�~�A�~�A݁A݃A݅A݁A݃A݇+A݉7A݇+A݉7A݇+A݇+A݉7A݋DAݍPAݏ\Aݏ\A݉7AݍPAݏ\Aݕ�AݓuAݏ\AݓuAݛ�Aݩ�A�ĜA�/A� �Aݏ\A��#Aܧ�Aܙ�A܁A�G�A�$�A��`A�A�A�dZA�C�A�Aӝ�A�(�A�?}A�ƨAϼjA�&�A΁A��HA̙�A�~�A�Q�A���A�A�jA�9XAŰ!A��A�r�A�|�A�ĜA�&�A�  A�Q�A�p�A��;A�5?A�=qA��+A�A�A���A�t�A�I�A�ffA�O�A�1'A�A�ZA�n�A���A���A�bNA���A�dZA�?}A���A�7LA�v�A��A�;dA���A���A���A��-A���A�
=A�^5A�?}A�ZA��#A��-A�ĜA���A���A�A�A��A��HA���A��!A�p�A�oA��Az�Ay�Ay%AxI�AwK�At�uAq�wAo�Anr�Am"�AkAhjAd�RAc
=Ab�A_VA[��AY�
AW�#AWAVI�ATn�ASO�ARE�AO��AK�AFA�AC\)A@�RA<�A8r�A6�yA4-A2�yA1/A01'A-�PA,A�A+��A)�A&�yA%dZA$�A#�PA#XA!��A!7LA ��A bNA=qA{At�A?}A=qA��AC�A��A �A�A�^A�yAZA^5AZAQ�A\)A{A~�A�HA%AA�A�A��A�;A��A+AQ�A��AG�A
�A	��A�RAbNA=qA=qA=qAAz�A\)AȴAE�AJA�mA��Ap�A33A��A��A�RA��Av�AI�A�A�A ��A �!A �@���@�@��j@�bN@�I�@�1'@���@��\@��@���@��@���@��h@�`B@�V@�;d@�?}@�p�@�9X@��@�F@�+@�^@�?}@���@���@��@�%@�u@��;@���@�^5@�=q@���@���@�^@�Ĝ@�Z@�1@ߍP@�33@�o@���@���@��y@���@ާ�@�v�@�^5@��T@�p�@�O�@�V@���@ܴ9@ܣ�@܋D@�I�@�(�@��;@�dZ@�M�@ش9@և+@�-@ղ-@�X@��@��/@�Ĝ@ԋD@�A�@�\)@҇+@��#@ѡ�@�`B@���@�bN@Ͼw@ϝ�@�|�@�S�@�+@���@���@͑h@̴9@�r�@�A�@�b@�ƨ@˕�@�;d@���@�n�@�5?@�{@��@�/@�Z@�t�@�@���@Ƨ�@�v�@�=q@�`B@Ĵ9@�Z@�o@��@���@�X@��@���@�Z@�l�@��@���@���@���@�{@���@���@��9@��u@�1@�;d@�
=@���@�^5@�$�@��@���@�hs@��@�z�@�j@�Q�@�I�@�1'@��@���@��T@��@�/@���@��/@�Ĝ@��9@���@�Z@�  @��P@�K�@��@�ȴ@���@���@��+@�ff@�J@��@�&�@�Ĝ@�1'@�K�@��R@��\@�v�@�E�@�@���@�O�@��D@��
@���@�5?@�O�@�I�@�K�@��@�E�@���@�7L@���@��j@��@�1'@�b@�K�@�~�@�ff@�V@�=q@��@��@��h@�`B@�&�@��@��@���@��@�1'@� �@��;@��H@�E�@�@�p�@�G�@�%@��@�j@�b@��m@��w@��P@��@�o@�
=@��y@���@�ȴ@���@�^5@�=q@��-@�O�@�G�@�7L@�7L@��@���@��9@���@�z�@�Z@�(�@�1@��@���@��P@���@���@�V@���@�G�@�%@���@���@�j@�9X@��@��F@�t�@�;d@��@�@��@���@��R@��!@���@���@��\@��+@�~�@�v�@�ff@�ff@�V@�$�@���@�G�@�Ĝ@�Q�@� �@���@���@���@��@�K�@��@��@�M�@��T@���@���@�`B@�?}@��@���@�r�@�j@�Q�@�b@��F@�|�@�+@���@�-@��#@��^@��-@���@���@��h@�x�@�p�@�p�@�X@�&�@��@��u@�I�@��
@��@��@�l�@�K�@�;d@�+@�@�ȴ@��!@�n�@�@��#@���@���@��^@���@��7@�p�@�`B@�7L@�V@��@�Ĝ@�r�@�A�@�(�@��@�b@�1@���@���@��m@��;@�|�@�+@���@�v�@�~�@�n�@�n�@�E�@�{@���@��T@��T@���@���@��^@��-@�p�@���@���@��@���@���@��@�z�@�r�@�Z@�A�@�(�@�1@�@�w@�P@~��@~��@~{@}�@|��@|�D@|Z@|�@{ƨ@{@z�H@z��@zM�@yhs@x�@w�w@w�P@w|�@v�R@vE�@v$�@u�@u�h@u?}@u/@u�@t�j@s��@r��@r=q@r-@q�#@q&�@p�9@p �@o|�@m/@l�j@l��@lj@lZ@lI�@kƨ@k33@j�@j-@i�#@i�#@i��@iX@hb@g�w@g|�@g+@g
=@g
=@f��@f�R@f��@f�+@dI�@bn�@a��@a�#@a��@a��@ax�@aX@a&�@`��@`Ĝ@_l�@^�+@^5?@]��@]�@]/@\�/@\�j@\�j@\j@[�F@Z�H@ZM�@Z�@Y�@Y7L@XĜ@W��@W+@V�@Vȴ@Vff@U@U��@UV@T��@TI�@S��@S��@St�@SC�@So@R�@R~�@Q��@P �@O�@O�P@Ol�@OK�@O+@N��@Nȴ@Nv�@M�@M�-@L�@Lz�@L�@K"�@J�\@J^5@J^5@JM�@J=q@I��@Ix�@I&�@H��@H�@HQ�@H1'@H1'@H1'@Hb@H  @G�;@G�@G�P@G|�@G;d@F��@F��@F$�@E�-@EV@Dz�@C�
@C��@CS�@C"�@C@B�@B��@B��@B~�@Bn�@Bn�@Bn�@B^5@B-@A��@A�7@Ahs@A&�@A%@A%@@�`@@Ĝ@@��@@��@@��@@��@@��@@�u@@�@@�@@�@@ �@@  @?�@?l�@?\)@?+@>�y@>�R@>v�@>5?@=�-@=p�@=`B@=O�@=�@<��@<I�@;��@;�m@;��@;t�@;C�@;"�@;o@;o@;@:��@:M�@9��@9hs@9X@97L@8��@8r�@8b@7�w@7�@7��@7+@7�@7
=@6��@6�y@6�y@6ȴ@6��@6V@5��@5O�@4�/@4Z@4�@3�m@3��@3@2��@2��@2�\@2M�@1x�@1%@0�9@0�u@0 �@/\)@.ȴ@.��@.��@.��@.��@.��@.ff@-@,�D@+�
@*��@)x�@)X@)&�@(Ĝ@(��@(�9@(�@'�@'�;@'�w@'��@'\)@'K�@'
=@&ff@%�@%�-@%�h@%O�@%V@$��@$Z@#�m@#��@#S�@#"�@#o@"��@"��@"J@!��@!��@!�^@!��@!�^@!��@!�7@!hs@!G�@!%@ ��@ Ĝ@ 1'@   @��@�w@�@�@�P@|�@l�@;d@
=@ȴ@v�@��@�@��@1@ƨ@�F@��@��@�@dZ@dZ@C�@33@33@o@�@�H@�!@M�@=q@-@-@=q@-@-@-@�@�@J@��@�#@7L@Ĝ@bN@�@�@��@��@�P@l�@�@��@��@��@�y@�@ff@$�@@�T@@@`B@V@�@z�@Z@I�@��@��@S�@33@@�\@M�@�@�@��@��@x�@x�@7L@��@��@�9@r�@A�@1'@ �@  @�w@�P@|�@l�@+@��@�@�R@�R@��G�O�A�x�A�z�A�x�A�z�A�t�A�v�A�v�A�t�A�v�A�x�A�z�A�z�A�z�A�x�A�z�A�|�A�|�A�z�A�~�A�|�A݁A݃A݃A݅A݇+A݉7A݅A݃A݃A݅A݅A݅A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݉7A݇+A݇+A݅A݅A݅A݅A݅A݇+A݇+A݇+A݇+A݇+A݅A݇+A݅A݇+A݅A݇+A݅A݅A݅A݅A݇+A݉7A݋DA݋DAݍPAݍPAݍPAݍPAݍPAݍPAݍPA݉7A݋DAݍPA݋DA݋DA݋DAݍPAݍPAݏ\Aݏ\Aݏ\AݍPAݑhAݑhAݏ\Aݏ\AݑhAݓuAݑhAݏ\Aݏ\AݍPAݍPAݍPAݍPAݏ\AݍPA݋DA݋DA݋DA݋DA݋DA݋DA݋DA݉7A݉7AݍPA݋DAݍPAݑhAݏ\Aݏ\A݋DA݋DA݋DA݇+AݍPAݍPAݓuAݗ�AݓuAݕ�Aݗ�Aݗ�Aݙ�Aݙ�Aݛ�Aݛ�Aݙ�Aݗ�AݑhAݑhAݍPAݍPAݍPA݋DA݉7Aݏ\AݍPAݓuAݑhAݑhAݍPAݓuAݕ�Aݕ�Aݕ�Aݏ\AݑhAݙ�Aݛ�Aݗ�Aݗ�AݓuAݗ�Aݟ�Aݡ�Aݝ�Aݝ�Aݝ�Aݥ�Aݣ�Aݝ�Aݧ�Aݟ�Aݥ�Aݣ�AݮAݲ-AݶFAݺ^Aݴ9AݮAݴ9AݾwA��#A��TA�A� �A�+A�/A�33A�5?A�;dA�=qA�=qA�=qA�;dA�1'A�+A�$�A��A�VA�A���A��A��HA�ȴAݓuA�hsA�I�A�9XA�-A��A���A��HA���A���AܾwAܴ9Aܰ!Aܩ�Aܧ�Aܩ�Aܧ�Aܧ�Aܧ�Aܧ�Aܡ�Aܛ�Aܙ�Aܗ�Aܗ�Aܗ�Aܕ�Aܕ�AܓuA܏\A܅A܃A�~�A�|�A�x�A�r�A�r�A�bNA�ZA�^5A�M�A�E�A�9XA�/A�+A�$�A�$�A�+A�(�A�+A�$�A��A��A��A�oA�bA�VA�A��A۩�A۝�Aۣ�AۑhA�v�A�jA�\)A�Q�A�C�A�{A��`A��
A�AڮAڛ�AڃA�jA�S�A�7LA�%A���AمA�;dA��A���Aؕ�A�5?AדuA�`BA�;dA��HA�z�A���Aԏ\A�33A��TA�ȴA���AӴ9AӲ-AӬAӣ�AӉ7A�hsA�A�A���A�ffA�33A���Aї�Aч+A�r�A�dZA�dZA�\)A�C�A�{A��mA��HA��#A��A���A���A���Aа!A�l�A� �A���A�ƨAϥ�AϋDA�~�A�v�A�r�A�bNA�M�A�oA��A���AμjAΕ�A΋DA΅A�jA�`BA�Q�A�5?A�-A��A��A͝�A�K�A�A���A�|�A�ZA�|�Ȁ\A̴9A̩�Aˡ�A�`BA�1'A��A���Aʴ9Aʧ�AʃA�;dA��A��A��yA���A�z�A��mA�O�A��
AǑhA�t�A�9XA�1A��AƶFAƥ�AƇ+AƇ+A�v�A�dZA�\)A�M�A�I�A�K�A�K�A�=qA�5?A�$�A�JA�VA���A��`A�ĜAŃA�A��#AĬA�I�A���A���Aç�A�t�A�33A��A��HA���A�ȴA�A�A�AA�ffA�S�A�C�A�;dA�7LA�33A�-A�(�A�"�A��A��A�bA�1A���A��A��;A��
A��FA��A�`BA�M�A�33A�"�A���A��A��HA���A��RA���A�^5A�-A��A��/A��A���A�A��-A���A��uA��+A�|�A�t�A�jA�\)A�VA�Q�A�O�A�K�A�A�A�=qA�7LA�7LA�-A�1'A�$�A��A��A� �A� �A��A�bA�A���A��A��`A��^A���A�z�A�x�A�ffA�XA�33A�  A��;A���A��FA���A��7A�t�A�bNA�Q�A�33A�bA��jA�bNA���A�"�A���A���A�ƨA���A��^A��9A��A���A���A���A���A��7A�~�A�~�A�x�A�p�A�l�A�jA�dZA�`BA�O�A�?}A�33A�/A�1'A�+A��A���A��A��A��9A��9A��!A���A���A���A��hA��A�z�A�t�A�p�A�VA�;dA�+A��A���A���A��A��mA���A��\A�ffA�S�A�K�A�;dA��A�bA���A���A��A��;A��A�A��RA���A��hA��A�jA�\)A�ZA�9XA��A��/A��RA���A��+A�Q�A��A��yA��;A�ȴA�t�A�M�A�/A� �A��A�{A�A�A�`BA�1'A�VA�  A��yA���A��FA���A�|�A�ZA�+A���A��;A���A��A�l�A�l�A�ffA�XA�C�A��A�oA�A���A��A���A��9A��A���A�v�A�G�A�bA��TA��;A��A�A��A�^5A��A��;A��7A��A�%A��;A��
A���A���A��RA��A�x�A�?}A�oA��HA��A��hA��A��A�z�A�jA�33A�(�A�{A�  A��
A���A�jA��A��HA���A�ƨA���A�x�A�I�A�1'A���A��mA���A�O�A�S�A�S�A�;dA�
=A�A�K�A���A�=qA�(�A��A�VA�JA�
=A�JA���A���A��+A�r�A�^5A��A���A��;A��
A�ƨA��FA��!A��-A���A���A�|�A�n�A�ffA�^5A�ZA�XA�VA�S�A�E�A�=qA�-A�$�A��A� �A���A���A��PA�7LA��HA�jA�1'A�VA��#A�`BA��A��HA��A�A��!A���A��\A��\A��hA��A�VA�-A��A�\)A�G�A�33A�(�A�bA�1A���A��A���A��A���A��A�-A���A���A�I�A��!A�l�A�=qA�"�A�  A���A�%A�1A��RA�~�A�5?A�A��A���A�S�A�VA�`BA�l�A�A�A�5?A�oA�`BA�G�A�A�A�A�S�A�-A���A���A��jA��7A��A� �A��A���A��hA�+A��A�&�A��A�JA�
=A�%A���A��A��#A���A���A�ȴA���A�A���A���A���A�ZA���A���A�5?A��A��A�t�A�/A�{A��/A��jA�r�A�E�A� �A��;A���A�t�A���A�/A�A�?}A�ƨA��A�bNA�C�A��A��A��A���A�M�A�A��FA�S�A��A��A�S�A�-A�
=A��A���A��FA���A��PA�\)A�+A��A��A��`A��;A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�\B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�'B	�\B	��B	�\B	�'B	��B	�-B	�bB	��B	�4B	�-B	��B	�hB	�B	�B	��B	�:B	��B	��B	��B
�B
?�B
=B
D�B
DgB
C�B
D3B
A�B
A�B
>BB
9�B
(XB
	B
�B
�B
YB
�B
�B
�B
@B
{B
�B
�B
,B
.IB
FtB
T�B
VB
UgB
^�B
y�B
��B
��B
��B
�B
�nB
�yB
��B
�B
�B
��B�B�B�B�BVBOB*�B+B+kB*eB.B(XB'�B$�B$tB"4B+BDBB
�.B
�B_B�B
	B
�B;B
�B
��B
��B
��B
�:B
��B
�B
x�B
[WB
EB
%FB
!�B
�B
B
7B
�B
hB
SB	�|B	�#B	�KB	��B	��B	�WB	ϫB	�}B	��B	��B	��B	��B	�~B	y>B	u%B	�4B	l�B	gmB	^5B	XEB	RTB	N<B	C-B	;dB	33B	!B	�B��B�;B�]B��B��B�}B�HB�dBȀB�BĜB�B�)B͟B�#BΥB��B�B�HB��BȴB�mB��B�B�pB�pB�TB�}BӏBԕBбB�}B�HB�mBޞB��B� B��B�fB�fB�B�`B�DB	�B	�B	B	�B	YB	#B	'B	,B	.�B	/�B	3hB	6B	6zB	6�B	7LB	6�B	6B	7LB	=B	?}B	@OB	A B	A�B	A�B	B'B	C�B	DgB	EmB	FB	FtB	F�B	G�B	H�B	I�B	K^B	Q�B	R�B	S�B	X�B	`B	cTB	b�B	b�B	bNB	dZB	h�B	jB	kQB	p;B	s�B	y>B	x�B	y	B	{B	u�B	iB	_pB	_�B	R�B	S�B	T�B	YB	\)B	_B	a|B	dZB	d&B	e�B	g�B	h
B	g�B	hsB	hsB	h
B	i�B	iDB	iDB	jB	jB	jB	i�B	i�B	i�B	i�B	i�B	jB	jB	m)B	n/B	n�B	o5B	o�B	p;B	qvB	rB	sB	sMB	s�B	tTB	v+B	{JB	�{B	�B	��B	��B	�SB	��B	�SB	�B	�SB	��B	��B	��B	�fB	�B	��B	�B	�~B	�~B	��B	��B	�PB	��B	��B	�uB	�B	�B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�\B	�B	�zB	��B	�_B	��B	��B	�0B	��B	�wB	��B	�!B	��B	��B	�*B	��B	�dB	�jB	��B	��B	��B	��B	� B	��B	��B	�gB	ǮB	�EB	�EB	��B	̘B	�6B	�B	�BB	�vB	�BB	��B	ѷB	��B	��B	��B	�aB	��B	��B	ԕB	�QB	یB	�]B	�dB	�dB	ݘB	��B	�5B	�B	��B	�B	�vB	�|B	��B	��B	��B	�TB	� B	�TB	��B	�2B	�fB	�B	�
B	�yB	�KB	�B	�B	�B	�B	�B	�B	��B	�WB	�/B	��B	�;B	��B	�B	�|B	��B	��B	�+B	��B	�2B	�2B	�lB	�B	��B	��B	��B	��B	��B	�B	�PB	�"B	��B	�VB	�"B	��B	��B	�]B	��B	�]B	��B
 iB
;B
B
AB
AB
�B
�B
{B
�B
�B
MB
�B
�B
SB
SB
�B
�B
�B
�B
�B
�B
_B
�B
_B
_B
_B
�B
�B
�B
�B
�B
fB
�B
�B
�B
�B
	lB
DB

rB
�B
~B
B
�B
�B
"B
VB
�B
(B
\B
�B
.B
.B
.B
bB
�B
�B
�B
�B
�B
�B
�B
 B
 B
�B
�B
�B
 B
oB
B
�B
�B
MB
B
SB
SB
SB
�B
�B
�B
�B
1B
�B
�B
7B
B
B
=B
	B
�B
=B
�B
CB
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
!B
�B
 'B
 �B
!bB
!bB
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"hB
#nB
#�B
#�B
#�B
#�B
#�B
#�B
$B
$@B
$B
$tB
$�B
$�B
%B
%�B
%�B
%�B
&B
&B
&B
&B
&B
%�B
%�B
&�B
'�B
(XB
(�B
(XB
(XB
(XB
(�B
(�B
)_B
)*B
)_B
)�B
)_B
)_B
)*B
*eB
+B
*�B
*�B
*�B
*�B
+6B
+kB
+6B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,qB
,=B
,�B
-�B
-�B
-�B
.B
-�B
.IB
/B
.�B
.�B
/�B
0!B
1'B
1�B
1'B
1[B
2-B
2aB
2-B
2aB
2�B
2�B
2�B
2aB
2�B
4B
4nB
4�B
4nB
5B
5?B
5?B
5�B
6B
9XB
8RB
8RB
8RB
8�B
8B
8�B
8�B
9�B
9�B
:*B
9�B
9�B
9�B
;�B
;dB
;dB
<6B
<6B
<B
<B
<6B
;�B
;0B
?B
?}B
?�B
?}B
?}B
?�B
?�B
?�B
?�B
?�B
?}B
A�B
AUB
A�B
A�B
A�B
B[B
B'B
B'B
A�B
B[B
B�B
C-B
C�B
C�B
C�B
DgB
D3B
E�B
FB
FtB
FB
GB
GEB
GEB
G�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IRB
I�B
K�B
K^B
K�B
K^B
K�B
K�B
K�B
K�B
L0B
L�B
L�B
NB
NB
NpB
OBB
OvB
O�B
OvB
OvB
OvB
PB
PB
QB
Q�B
Q�B
Q�B
R B
R B
Q�B
RTB
R B
R B
RTB
R�B
RTB
R�B
R�B
S&B
S�B
S�B
T�B
T�B
U�B
U�B
VB
VB
V9B
VB
VB
V�B
V�B
V�B
VmB
VmB
V�B
V�B
W
B
W
B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
XyB
X�B
XyB
X�B
YB
YKB
YKB
YB
ZB
ZB
ZB
ZB
ZQB
ZB
[#B
Z�B
Z�B
[WB
[WB
[�B
[�B
[WB
[�B
[WB
[�B
\)B
\�B
\�B
\�B
\�B
]�B
\�B
^5B
^5B
]�B
^B
_B
^jB
^�B
^�B
^jB
^jB
^jB
^�B
^�B
_;B
_�B
`BB
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a|B
a�B
b�B
c B
b�B
c B
cTB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
f�B
h
B
h>B
hsB
hsB
iyB
iDB
iB
iB
i�B
iB
iyB
iB
i�B
iB
i�B
i�B
j�B
j�B
jB
j�B
kQB
kB
j�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
m�B
m�B
n/B
m�B
n/B
o5B
o5B
oiB
o�B
oiB
oiB
o�B
o�B
o�B
pB
p;B
pB
p�B
qB
rB
r�B
sMB
sB
s�B
s�B
r�B
s�B
s�B
sMB
tB
s�B
s�B
s�B
s�B
tTB
s�B
tTB
t�B
t�B
tTB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
tTB
tB
u%B
uZB
v`B
v�B
v�B
v�B
v+B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
xlB
xlB
xlB
xlB
xlB
xB
xlB
x�B
yrB
yrB
y�B
yrB
yrB
y�B
z�B
z�B
z�B
z�B
{B
{JB
{JB
{�B
|PB
|B
{B
|B
|�B
|�B
}"B
|�B
|�B
}"B
}�B
}�B
}�B
}VB
}"B
}�B
~�B
.B
.B
~�B
~�B
.B
cB	��B	��B	��B	�-B	��B	��B	��B	�\B	��B	��B	�B	�!B	��B	�!B	��B	�\B	��B	��B	�'B	��B	�!B	��B	��B	��B	�VB	��B	��B	��B	��B	��B	��B	�OB	�VB	��B	�VB	��B	��B	��B	��B	��B	��B	�\B	��B	�-B	��B	��B	��B	��B	��B	��B	�bB	��B	��B	��B	�-B	��B	��B	�-B	��B	��B	�-B	��B	��B	�VB	��B	�'B	��B	��B	��B	��B	��B	�\B	�\B	��B	��B	�hB	�4B	��B	��B	��B	�-B	��B	�-B	��B	��B	��B	��B	�-B	��B	�\B	��B	��B	�4B	�bB	��B	��B	�:B	�B	�:B	��B	�B	�B	��B	��B	��B	��B	��B	�\B	��B	��B	�!B	��B	�'B	�:B	��B	�tB	�hB	�@B	��B	��B	��B	�:B	�VB	��B	��B	�bB	�nB	�nB	��B	�B	�B	��B	��B	�@B	�:B	�:B	��B	��B	�tB	�B	��B	��B	��B	�bB	�B	��B	�-B	�4B	��B	�nB	�B	��B	�bB	�\B	��B	�B	��B	�B	�nB	�B	��B	�[B	��B	�RB	�*B	�eB	��B	�wB	�$B	��B	�CB	�*B	�=B	�=B	�'B	�}B	�=B	��B	��B	�B	��B	�B	��B
�B
"B
4B
�B
!�B
2aB
5B
9�B
?�B
?}B
@B
A B
A B
B'B
>�B
=�B
>B
=�B
?}B
C�B
?B
9�B
9�B
;�B
A�B
B[B
B�B
E9B
E�B
G�B
FtB
D�B
F?B
C�B
C�B
EB
C-B
B�B
C�B
EB
DgB
D�B
DgB
DgB
D�B
D�B
EB
D�B
DgB
FB
D�B
D�B
EmB
D�B
EB
DgB
HB
AUB
B�B
HB
@B
@�B
?HB
@B
@B
A�B
A�B
C-B
C-B
C�B
A B
A�B
@OB
A�B
A B
A�B
B�B
E9B
:�B
4�B
5tB
;�B
8�B
9�B
5tB
6FB
<6B
<6B
7B
0�B
/�B
+�B
-�B
%zB
&LB
 \B
#B
�B
 �B
�B
+B
�B
�B
{B
eB	��B
bB
�B
B
MB
�B
#nB
SB
�B
�B
�B
�B
�B
�B
�B
 B
@B
�B
'�B
xB
�B
+B
�B
xB
�B
VB
�B
�B
�B
_B
�B
�B
�B
~B
xB
DB
xB
\B
{B
�B
bB
�B
FB
:B
bB
�B
\B
B
�B
�B
�B
B
B
eB
B
B
�B
�B
�B
�B
uB
{B
�B
!-B
&�B
�B
$@B
	B
~B
�B
=B
1B
 'B
,�B
%FB
0�B
0�B
/�B
)�B
)�B
0!B
1�B
0UB
.B
.B
2�B
A�B
I�B
L�B
\�B
K^B
QB
W
B
TaB
V�B
TaB
XEB
Z�B
S�B
W�B
TaB
T�B
VmB
S&B
S[B
S�B
U�B
XEB
WsB
X�B
T�B
YB
[#B
\�B
c�B
kQB
m)B
o5B
�B
w�B
tB
}VB
~]B
�GB
�B
��B
�B
�oB
�	B
�B
��B
��B
�YB
��B
�+B
�%B
��B
��B
��B
�MB
�MB
��B
�MB
�GB
�MB
�B
��B
��B
��B
��B
��B
�~B
��B
�B
��B
� B
��B
��B
��B
�FB
��B
��B
�B
�B
�B
��B
�xB
��B
��B
�B
�B
�bB
��B
�B
�B
��B
��B
�B
�B
��B
�zB
��B
��B
��B
��B
�B
��B
��B
�FB
��B
��B
�B
��B
�RB
��B
��B
�_B
��B
�0B
�OB
�kB
��B
�}B
��B
��B
�zB
�LB
�zB
��B
�$B
��B
�XB
��B
��B
��B
�9B
��B
�KB
�]B
��B
��B
��B
��B
�]B
ܒB
ݘB
ޞB
�dB
�dB
�B
�B
ߤB
�B
�vB
��B
�|B
�B
�vB
�B
�B
��B
��B
�B
�B
��B
��B
��B
�sB
��B
��B
��B
�QB
�B
�WB
�B
��B
�WB
��B
�WB
�)B
�B
�B
�+B
�2B
�GB
��B
�B
�B
��B
��B
�VB
��B
�rB
��B
�B
��B
��B
�(B
��B iBB  BB�B  BBB�B�B �BxB�BVB
=B
=B�B�BbB�B:B�B@BB�B4B�BBIB�B�BB1BB�B�BB \B�B%BqB!-B%�B#nB �B�BB~B \B�BIBBkB�B�B�BqB�BB!�B"�B!B�B�B#�B'RB!bB)�B'RB+kB?HB:�B'�B'B$�B%�B&B%�B)�B+�B.}B-�B-�B-B*�B'�B'B*�B,qB'�B)�B'RB,�B.�B.}B1�B-�B)*B)�B*�B.IB#nB'�B%�B,�B8BAUB&�B(�B�B*0B-�B3�B-�B=�B)�B)_B'B&�B&LB$�B(XB/�B(XB%�B)�B+�B)�B'�B%zB'B(XB&B$B%�B'�B)*B&�B$�B$�B$tB$@B"4B"4B$B"4B%B'B!�B �B&LB#�B%FB"hB0�B<�B"�B!�B!�B,=B+�B�B�BCB�B�B�BFB�BSBeB�B(�BBB B"B~BB
=B	�BB	7B�B�B�B
��B
��B�BPBAB
�.B
�B
�.B
��B
�>B
�DBoB�B�B{B
��B
�+B
��B
�ZB
�B
� B
��B
�B
��B�B
��B
��BYB�B�B	7B_B�B1B�BSB
��BB@B
�(B
�DBfB�B	�B�B	B
rBDB�B
�BJB
�B	B
=B�B�BYBB"B�B
�B
��B
�B�B
��B
�B
�]B
�fB
�ZB
�5B
�B
�B
�B
��B
��B
ܒB
�NB
یB
�)B
�B
ǮB
��B
�[B
�gB
�RB
�B
��B
�RB
�3B
�B
�nB
��B
��B
�B
�	B
��B
��B
�FB
�@B
��B
��B
��B
�B
�4B
��B
��B
�_B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        B	�eB	�eB	��B	��B	�yB	��B	�yB	�_B	��B	��B	��B	��B	�1B	�B	�1B	��B	�1B	��B	�eB	�B	�7B	�kB	�#B	�B	�kB	�#B	��B	��B	��B	��B	�5B	�4B	�6B
�B
;JB
9�B
>wB
=�B
="B
>(B
;0B
;�B
:�B
7�B
-�B
&2B
�B
bB
�B
�B
�B
B
�B
NB
�B
�B
+�B
/�B
H�B
P�B
PHB
Q�B
a-B
z�B
��B
�6B
�xB
�LB
�B
�SB
�B
��B
�B
��BGB BYBIBB#TB/�B+QB.�B.}B0!B'B#nB"B*�B"4B�B�B�B �BBB�B�B\B�B
��B
��B
�B
�!B
�<B
� B
� B
~wB
_�B
G�B
�B
)B
?B
�B
�B
B
�B
jB	�;B	յB	ԯB	ӏB	ݘB	�B	�<B	�VB	�%B	��B	�DB	�|B	��B	tB	z�B	��B	l�B	f�B	ZB	TB	Q4B	KB	@4B	=�B	9>B	*�B	�B��B��B��B��B��B�VB��B�RB�rB�jB��B�1B��B��BǔBɠB��B˒B��BāB��B��B�HB�xBȀB��B�B�xB��B�(B�rB�rB�0B�NB׍B��BܒB�VB�&B�FB��B�B�hB��B	�B	
�B	�B	�B	OB	#B	'RB	)*B	+�B	/�B	2GB	0�B	0!B	0UB	0B	1B	4�B	9�B	:�B	;B	:�B	:�B	;B	<�B	=�B	>BB	>�B	?cB	?�B	@iB	A�B	B�B	C�B	F�B	K�B	LdB	OBB	T�B	Z�B	\�B	\B	[�B	\xB	_!B	bhB	dtB	gB	k�B	oB	r�B	r�B	uZB	w�B	uB	dtB	[WB	^B	NpB	M�B	N�B	R�B	U2B	W�B	Z�B	^B	^jB	`�B	aHB	aHB	aHB	a�B	a�B	b�B	c�B	b�B	c B	dB	cTB	c:B	b�B	b�B	cB	c B	cB	c�B	dZB	f�B	gmB	h
B	h�B	h�B	i_B	j�B	k�B	lWB	l�B	m�B	o�B	r-B	xB	}"B	|�B	~B	~BB	~�B	~�B	~�B	~�B	�B	�;B	��B	�'B	��B	��B	��B	�B	��B	��B	��B	�%B	��B	�EB	�B	��B	��B	�vB	�B	� B	��B	��B	�@B	��B	��B	��B	�KB	��B	��B	�B	��B	��B	��B	�TB	��B	�,B	��B	�DB	�kB	��B	�B	��B	�TB	��B	�FB	�8B	�DB	��B	�*B	�xB	�B	��B	��B	��B	��B	�;B	�B	��B	��B	�fB	ȚB	ȀB	ȀB	��B	��B	�6B	��B	�B	�jB	�B	�PB	ϫB	��B	�2B	��B	֡B	ևB	ּB	��B	�?B	׍B	�_B	��B	��B	�	B	�B	��B	�B	�xB	�]B	��B	ޞB	��B	�B	�B	�B	�nB	�B	�:B	�nB	��B	��B	�ZB	��B	�RB	�B	�>B	�B	�6B	��B	�"B	��B	��B	�B	�B	��B	��B	�B	�B	�|B	�+B	�B	��B	�B	�B	�ZB	��B	�`B	�FB	�`B	�B	�B	��B	��B	��B	��B	�XB	�xB	�B	��B	�B	��B	��B	��B	�B	�"B	�"B	��B	�(B	�wB	�BB	�wB	��B	��B	��B	�.B	�.B	��B
 �B
 �B
 iB
 OB
 �B
B
 �B
 �B
 B
 B
�B
�B
�B
�B
[B
�B
gB
�B
�B
B
tB
�B
EB
_B
�B
1B
�B
�B
�B
	RB
	7B
	7B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

	B
	�B
	�B

#B

�B
B
�B
pB
B
�B
\B
\B
\B
�B
.B
�B
B
oB
:B
:B
B
[B
@B
�B
{B
B
�B
�B
gB
�B
9B
mB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
+B
yB
KB
�B
QB
�B
�B
�B
�B
�B
�B
	B
WB
�B
�B
B
�B
�B
�B
�B
�B
�B
B
IB
IB
�B
�B
B
�B
�B
�B
�B
B
B
!B
B
!B
�B
VB
 vB
!bB
!|B
!bB
!bB
!HB
!�B
!�B
!�B
"hB
"B
"hB
"�B
"hB
"hB
"�B
$&B
#�B
#�B
#�B
#�B
#�B
$tB
$ZB
$@B
$�B
$�B
$�B
$�B
$�B
$�B
%FB
%�B
%�B
&�B
'B
&�B
&�B
'8B
'B
'�B
($B
'�B
($B
)DB
)�B
*�B
*�B
*0B
+B
+�B
+kB
+QB
+�B
+�B
+�B
+�B
+�B
-B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
1B
2�B
1[B
1[B
1AB
1�B
1�B
2-B
2-B
3B
2�B
3B
2�B
3MB
4B
5B
4�B
4�B
5?B
5%B
4�B
5%B
5%B
4�B
6FB
9�B
8�B
8�B
8lB
8�B
8�B
8�B
8�B
9	B
9	B
9�B
;B
:�B
:�B
:�B
:�B
;B
;0B
;B
;0B
<B
<jB
<�B
<�B
<�B
="B
=�B
=�B
?B
?.B
?cB
?cB
@�B
@OB
@�B
AB
AB
A�B
A�B
A�B
A�B
A�B
A�B
BB
CB
DB
EB
DgB
D�B
DgB
D�B
D�B
D�B
E9B
E�B
E�B
F?B
G_B
G_B
H1B
H�B
H�B
H�B
HfB
HfB
H�B
I7B
IRB
JXB
J�B
J�B
J�B
KB
KB
J�B
KDB
K)B
K)B
K^B
KxB
KxB
K�B
K�B
L~B
L�B
MPB
NB
NpB
N�B
N�B
OB
OB
O(B
OB
OB
O�B
O�B
OvB
O\B
O\B
O�B
O�B
P.B
PB
PbB
P}B
PbB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QB
QhB
Q�B
Q�B
Q�B
RB
R B
RoB
RoB
R�B
S@B
SB
SB
S&B
S[B
S�B
TFB
S�B
TB
TaB
TaB
T�B
T{B
TFB
T{B
T{B
T�B
U�B
VB
U�B
U�B
VB
V�B
VB
WYB
W$B
V�B
W?B
W�B
WYB
W�B
W�B
WYB
WsB
WsB
W�B
X_B
XyB
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\CB
[�B
\xB
\�B
]�B
]�B
]~B
]�B
]�B
]~B
]�B
^B
_pB
_pB
`�B
a�B
aHB
a|B
a�B
bhB
bB
b4B
b�B
b�B
bB
b�B
b4B
b�B
b4B
c B
c:B
dB
c�B
c�B
c�B
d�B
d@B
d@B
d�B
eB
d�B
d�B
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
f�B
gB
gB
gB
g�B
h>B
h>B
hXB
h�B
hXB
hsB
h�B
h�B
h�B
iB
i_B
iDB
jKB
jKB
k�B
l"B
lqB
lB
lqB
lWB
k�B
l�B
lqB
lWB
mB
l�B
l�B
l�B
l�B
m]B
m)B
mCB
m�B
m�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
m]B
m�B
n}B
n�B
o�B
o�B
o�B
o�B
oB
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
qvB
qvB
qvB
q[B
qAB
q�B
rB
r|B
r|B
r�B
r�B
r�B
r�B
s�B
s�B
tB
s�B
t�B
tTB
tTB
t�B
uZB
uB
t�B
u?B
utB
u�B
vFB
u�B
u�B
vB
v�B
v�B
v�B
vFB
vB
v�B
w�B
x8B
x8B
w�B
w�B
xG�O�B	�kB	�kB	��B	�B	�kB	��B	��B	�1B	�_B	��B	��B	��B	��B	��B	�_B	�1B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	�YB	��B	��B	��B	�YB	�$B	�+B	�YB	�+B	��B	��B	��B	�eB	��B	��B	�1B	��B	�B	��B	��B	�kB	�kB	�eB	��B	�7B	��B	�kB	��B	�B	��B	��B	�B	�eB	��B	�B	��B	��B	�+B	��B	��B	��B	��B	��B	��B	��B	�1B	�1B	�eB	��B	�=B	�	B	�qB	��B	��B	�B	�kB	�B	��B	��B	��B	�eB	�B	��B	�1B	��B	��B	�	B	�7B	�qB	��B	�B	��B	�B	��B	��B	��B	��B	�eB	�_B	��B	�_B	�1B	�_B	�eB	��B	�_B	��B	�B	�qB	�IB	�=B	�B	�qB	��B	��B	�B	�+B	��B	�kB	�7B	�CB	�CB	�qB	��B	��B	��B	�xB	�B	�B	�B	��B	�xB	�IB	��B	�qB	��B	��B	�7B	��B	��B	�B	�	B	��B	�CB	��B	�kB	�7B	�1B	��B	��B	��B	��B	�CB	��B	�\B	�0B	��B	�'B	��B	�:B	��B	�LB	��B	��B	�B	��B	�B	�B	��B	�RB	�B	��B	�wB	��B	��B	��B	�vB	��B
�B
	�B
[B
QB
+B
-�B
2|B
8�B
88B
8�B
9�B
9�B
:�B
7�B
6`B
6�B
6`B
88B
<PB
7�B
2GB
2|B
4TB
:xB
;B
;�B
=�B
>]B
@iB
?.B
=�B
>�B
<�B
<�B
=�B
;�B
;�B
<PB
=�B
="B
=VB
="B
="B
=VB
=�B
=�B
=�B
="B
>�B
=�B
=�B
>(B
=�B
=�B
="B
@�B
:B
;JB
@�B
8�B
9>B
8B
8�B
8�B
:DB
:DB
;�B
;�B
<�B
9�B
:�B
9	B
:DB
9�B
:DB
;B
=�B
3MB
-]B
./B
4TB
1vB
2|B
./B
/ B
4�B
4�B
/�B
)yB
(�B
$ZB
&fB
5B
B
B
�B
�B
�B
?B
�B
vB
	�B
6B
 B	�B
Z�B
�B
�B
B
�B
)B
B
�B
�B
�B
�B
�B
�B
fB
	�B
�B
�B
 BB
2B
�B
�B
	lB
MB
�B
+B
EB
?B
�B
B
�B
�B
�B
SB
MB
B
MB
1B
6B
^B
	B
^B
B

�B
	B
�B
1B

�B
�B
<B
BB
�B
�B
 B
�B
�B
}B
dB
pB
BB
JB
PB
�B
�B
;B
EB
�B
�B
9B
�B
�B
�B
�B
%�B
B
)DB
)yB
(sB
"NB
"NB
(�B
*B
)B
&�B
&�B
+�B
:xB
B�B
ESB
U�B
DB
I�B
O�B
MB
O�B
MB
Q B
S�B
LJB
P�B
MB
M�B
O(B
K�B
LB
L�B
NVB
Q B
P.B
QhB
MPB
Q�B
S�B
UMB
\xB
dB
e�B
g�B
x�B
p�B
l�B
vB
wB
|B
|�B
|jB
z�B
z*B
��B
{�B
{dB
~BB
B
}B
�B
~�B
~�B
~BB
~BB
}B
}B
|�B
}B
|B
}B
|�B
~BB
}B
}qB
�UB
�zB
�9B
�gB
��B
�tB
��B
��B
�RB
�XB
�B
�}B
��B
��B
��B
��B
��B
�2B
�sB
�EB
��B
��B
�B
��B
��B
��B
�vB
��B
��B
��B
��B
�5B
��B
�;B
�;B
�|B
��B
�|B
��B
�B
�dB
�dB
��B
�pB
�B
�HB
�BB
�B
��B
��B
�
B
�&B
�yB
�8B
�]B
�GB
�5B
�B
�5B
�MB
��B
��B
�B
�GB
��B
�>B
��B
ňB
�B
�B
ٚB
ԯB
ԯB
ԯB
�B
�MB
�SB
�YB
�B
�B
ּB
��B
�_B
��B
�1B
ڠB
�7B
��B
�1B
��B
�CB
�~B
ބB
�CB
�kB
ۦB
ބB
�B
�-B
�B
�B
�B
�B
�nB
�B
�@B
�B
�B
�B
�B
��B
�=B
��B
��B
��B
�B
�B
�CB
��B
�B
�B
�B
��B
�-B
�?B
��B
�hB
�FB
��B
�RB
�$B
��B
��B
��B
��B
��B
��B
��B
�dB
�<B
�XB3B OBB�B�BjB�B	BEB
�B<B�B�BdB	�B	�B�BBaB�B�B�B�B�BTB�BBmB�B,B�BjB)BKBsB�B9BB�BB�B&BmBsB�B,B�B�B�B�B�BgB[B]B BB"NB B$&B8B3MB BB�B�BjB�BjB"�B$�B'8B&�B&�B%�B#TB vB�B#TB%,B �B"�B B%�B'�B'8B*B&fB!�B"�B#�B'B)B �BjB%�B0�B:B�B!�BEB"�B&�B,�B&�B6`B"�B"B�B;BB�B!B(�B!BjB"�B$�B"�B vB5B�B!B�B�B�B �B!�BpB�B�B/B�B�B�B�B�B�B�BQBKBB�BB#B)�B5�B�B�BQB$�B$�B�B�B�B�B[B�BBdBB BjB!|B�B
�B	�B�B9B�B�B[B�B�B�B
�dB
�wB
�RB
�tB
�wBB
��B
��B
��B
��B
�[B
��B
��B)B
�jB
�wB
�6B
�B
��B
�B
�B
�=B
�B
�B
��B
�tBtB
�?B
�B
�B �BUB�B B
��B �B�BB
�B
��B�B
��B
��B B�B[BUB�B-B�B�B�BBaB�B�B �B
�HB
�B�B�BKB�B
�FB
��B
��B
�B
��B
�B
�!B
�B
��B
�nB
��B
�kB
ۦB
��B
�MB
�	B
�FB
��B
��B
�iB
��B
�B
�"B
�B
��B
�>B
�B
��B
��B
�)B
�vB
�?B
��B
��B
�}B
��B
�B
��B
��B
��B
�XB
��B
��B
�OB
��B
�B
~BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<W�}<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.a�<#�
<#�
<#�
<22�<#�
<#�
<.��<#�
<#�
<|��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<W(G<V]=<#�
<=�F<I�<+�<#�
<#�
<#�
<j�q<#�
<#�
<Cj3<B�k<#�
<Tl�<~��<#�
<#�
<O�<N��<���<F��<Z6h<#�
<#�
<#�
<#�
<`��<L��<2<#�
<#�
<#�
<#�
<#�
<#�
<j�\<��t<#�
<#�
<#�
<#�
<&-�<$�h<#�
<#�
<#�
<#�
<J�<N)�<#�
<#�
<`R�<=��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)*Y<b�<���<:2<'�Y<Z� <x��<#�
<'7�<#�
<#�
<#�
<&��<#�
<#�
<(@B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.0049)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.0049)                                                                                                                      SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2020111922013120201119220131IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400402120210224004021QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400402120210224004021QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714015120210427140151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                